{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent             as Co
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Random                  as Cr
import           Data.Aeson                     as Ae
import           Data.Bifunctor                 as Bf
import           Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BSL
import           Data.List                      as List
import           Data.Map.Strict                as Map
import           Data.Maybe
import           Data.Text                      as Text
import           Data.Word
import           Network.HTTP.Client
import           Network.HTTP.Types             as HT
import           Network.Wai                    as Wai
import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets as WW
import           Network.WebSockets             as WS
import           Options.Applicative            as Opt

import           Core
import           Lnd
import           Types                          hiding (Config)
import qualified Types

-- ~~~~~ --
-- Setup --
-- ~~~~~ --

data Config
    = Config
    { servicePort           :: Int
    -- ^ the server should listen for connections here
    , dataFile              :: FilePath
    -- ^ where to write the JSON representation of the state
    , sweepInterval         :: Int
    , saveInterval          :: Int
    , logLevel              :: LogLevel

    , configSurveyPrice     :: Word32
    -- ^ satoshis per survey
    , configBlobStorageRate :: Word32
    -- ^ this is in satoshis per day

    , macaroonPath          :: FilePath
    , certPath              :: FilePath
    , lndHost               :: ByteString
    , lndPort               :: Int
    } deriving (Eq, Show)


-- | Options parser corresponding to Config
progOptions =
    Config <$> port <*> dataFileO <*> sweep <*> save <*> level <*>
    sPrice <*> blobRate <*>
    macPath <*> certPath <*> host <*> portL

    where

    port = option auto $ long "port" <> short 'p' <> value 3123
    dataFileO = strOption $ long "dataFile" <> short 'd' <> value "data.json"
    sweep = option auto $ long "sweepInterval" <> value (5 * 10^6)
    save = option auto $ long "saveInterval" <> short 's' <> value (30 * 10^6)
    level = option auto $ long "logLevel" <> short 'v' <> value LevelError

    sPrice = option auto $ long "surveyPrice" <> value 130000
    blobRate = option auto $ long "blobRate" <> value 10000

    macPath = strOption $ long "macaroonPath" <> short 'm'
    certPath = strOption $ long "certPath" <> short 'c'
    host = strOption $ long "host" <> short 'h' <> value "localhost"
    portL = option auto $ long "lndPort" <> value 10009


-- | This name is to unwrap from a logging context
runLog = runStderrLoggingT

trySendTextData :: WS.Connection -> BSL.ByteString -> IO (Either ConnectionException ())
trySendTextData conn = try . WS.sendTextData conn

-- ~~~~~~~~~~~ --
-- Entry point --
-- ~~~~~~~~~~~ --


main :: IO ()
main =
    execParser (info (progOptions <**> helper) fullDesc) >>= \Config{..} ->

    -- try to read the state from the data file
    --
    -- FIXME if the file is not there this crashes
    Ae.decode . BSL.fromStrict <$> BS.readFile dataFile >>= \rawState ->

    -- if there is a file but the data doesn't parse, start with empty state
    -- and a random administrative key
    let randomState = emptyState . Key <$> Cr.getRandomBytes 32 in
    maybe randomState return rawState >>=

    newMVar >>= \appS ->

    let params = lndParams certPath macaroonPath lndPort lndHost
        pricing = Pricing (Satoshis configSurveyPrice) (Satoshis configBlobStorageRate)
    in
    runExceptT params >>= \case

        Left err -> print err

        Right lndP ->
            -- client connections
            let server =
                    acceptRequest >=>
                    (\conn -> WS.forkPingThread conn 30 >> return conn) >=>
                    websocketServer pricing lndP appS
                webApp = WW.websocketsOr defaultConnectionOptions server fallback
                fallback _ respond = respond $ responseLBS status400 [] "websocket requests only"
            in
            forkIO (Warp.run servicePort webApp) >>

            -- write down the state
            forkIO (simplePersistence appS saveInterval dataFile) >>

            -- we are not using a streaming connection to LND for real time
            -- payment channel updates, so we poll periodically
            let sweep = catchError (sweeper appS) $ \err ->
                            logErrorN (tShow err)
                unpack = flip runReaderT lndP . runExceptT

            in
            runLog $ forever $
                unpack sweep >>
                liftIO (threadDelay sweepInterval)


-- | Encode the state as a JSON document and write to a file
simplePersistence appS saveInterval path = runLog $ forever $
    let go = readMVar appS >>= \state ->
            BS.writeFile path (BSL.toStrict $ Ae.encode state) >>
            threadDelay saveInterval
    in
    logDebugN "Writing state" >>
    liftIO go


-- | This layer is responsible for packing the required operations into a
-- simple API
websocketServer pricing lndP appS conn =

    let receiveM = liftIO (WS.receiveData conn) >>= \raw ->
            logDebugN (tShow raw) >>
            return raw


        sendM = liftIO . trySendTextData conn . Ae.encode

        run = flip runReaderT lndP . runExceptT

        wait = threadDelay $ 2 * 10 ^ 6

        watchMailbox key = forever $ takeMVar appS >>= \state@AppState{..} ->
            let deliver = maybe (return ()) $ mapM_ sendMsg
                sendMsg = trySendTextData conn . Ae.encode . SessionMessage 0 Nothing
                state' = state { appMailbox = Map.delete key appMailbox }
            in
            deliver (Map.lookup key appMailbox) >>
            putMVar appS state' >>
            wait

    in
    void . runLog $
    logDebugN "New connection" >>
    run (simpleServer pricing appS receiveM sendM watchMailbox)


-- | This server expresses the main logic of the websocket service
simpleServer pricing appS receiveMessage sendMessage watchMailbox =
    liftIO (readMVar appS) >>= \AppState{..} ->

    -- Start by sending some public objects
    let surveys = Types.Object . toJSON . first (IdT SurveyT) <$> Map.toList appSurveys

        items = Types.Object . toJSON . first (IdT ItemT) <$> Map.toList appItems

        sendObject msg =
            get >>= \n ->
            put (n+1) >>
            let smsg = SessionMessage n Nothing msg in
            lift (sendMessage smsg) >>=
            let recover (e :: ConnectionException) = liftIO (print e) >> put n in
            either recover (const $ return ())
    in
    execStateT (mapM sendObject $ surveys ++ items) 1 >>= \n ->

    -- Wait for the user send their session key
    establish >>= \key ->

    -- Route messages coming from elsewhere to this user
    liftIO (forkIO $ watchMailbox key) >>

    -- Main protocol loop
    loop key n

    where

    establish = receiveMessage >>= \raw ->
        case Ae.eitherDecode raw :: Either String (SessionMessage Server) of
            Left err ->
                logDebugN (Text.pack err) >>
                establish

            Right (SessionMessage i _ (Sync key)) ->
                logDebugN (tShow key) >>
                sendMessage (SessionMessage 0 (Just i) Ack) >>
                return key

            _ -> establish

    loop key n = let loop' = loop key in
        receiveMessage >>= \raw ->
            case Ae.eitherDecode raw of
                Left err ->
                    let logText = "Decoding error! " <> Text.pack err in
                    logDebugN logText >>
                    loop' n

                Right (SessionMessage cn ref msg) ->
                    logDebugN "Handling message" >>

                    -- run the protocol in the straightforward way
                    let handleMessage =
                            runApp keyGuard emit noop (protocol pricing msg) >>
                            saveState appS >>
                            lift (loop' (n+1))

                        emit = (>>= either recover (const $ return ())) . sendMessage . SessionMessage n (Just cn)
                        recover e = logErrorN (tShow e)

                        noop = return ()
                        keyGuard = ($ key)

                        cleanup err =
                            logErrorN (tShow err) >>
                            -- Revert state
                            saveState appS >>
                            lift (loop' n)

                    in
                    runStateMVar appS
                    (catchError handleMessage cleanup)


-- | Sweep for payments and take the appropriate action
sweeper appS =

    logDebugN "START sweep" >>

    let op h = catchError (runApp keyGuard emit noop $ sweepOne h) $ \err ->
            logErrorN (tShow err) >> return []

        emit = return . (:[])
        noop = return [Ack]
        keyGuard _ = return [Ack]

        sweep = get >>= \AppState{..} -> mapM op $ Map.keys appInvoiced

        enqueueClientMessage state@AppState{..} = \case
            msg@(Confirmation _ (Just h)) ->
                let f key = state { appMailbox = Map.insertWith (++) key [msg] appMailbox } in
                maybe state f $ Map.lookup h appOwners
            _ -> state

        postMessages msgs = modify $ \state0 -> List.foldl enqueueClientMessage state0 msgs

    in
    runStateMVar appS (sweep >>= postMessages . join >> saveState appS) >>
    logDebugN "END sweep"


-- an interface for keeping state between threads with an MVar

runStateMVar :: MonadIO m => MVar a -> StateT a m r -> m r
runStateMVar v x = liftIO (takeMVar v) >>= evalStateT x

saveState :: MonadIO m => MVar a -> StateT a m ()
saveState v = get >>= liftIO . putMVar v


-- an inconsistently used helper
tShow :: Show a => a -> Text
tShow = Text.pack . show




-- |
-- module: Core
--


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Core (
    AppState(..)
    , emptyState
    , protocol
    , AppST
    , runApp
    , sweepOne
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens           as Lens
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.State
import           Crypto.Hash
import           Data.Aeson             as Ae hiding (Object)
import           Data.Aeson.Lens
import           Data.ByteArray         as BA
import           Data.ByteString        as BS
import           Data.ByteString.Base64 as B64
import           Data.ByteString.Char8  as BS8
import           Data.ByteString.Lazy   as BSL hiding (ByteString)
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.List              as List
import           Data.Map.Strict        as Map
import           Data.Maybe             as Maybe
import           Data.Text              as Text
import           Data.Time              as Time
import           Data.Tuple
import qualified Data.Vector            as Vector
import           Data.Word

import           Lnd
import           Types


-- ~~~~~ --
-- State --
-- ~~~~~ --


-- | The state model is purely to guide the development process and the entire
-- state will not generally be loaded into memory.
data AppState
    = AppState
    { appItems         :: Map (Id Item) Item
    , appOrders        :: Map (Id Order) Order
    , appContributions :: Map (Id Contribution) Contribution
    , appSurveys       :: Map (Id Survey) Survey
    , appInvoiced      :: Map ByteString PaidFor
    , appBlobs         :: Map (Id Blob) Blob

    , appOwners        :: Map ByteString Key
    , appMailbox       :: Map Key [MessageTo Client]
    , appAdminKey      :: Key
    }

instance ToJSON AppState where
    toJSON AppState{..} = object
        [ ("items", jsonMap appItems)
        , ("orders", jsonMap appOrders)
        , ("invoiced", invoiced)
        , ("blobs", bsJsonMap appBlobs)
        , ("contributions", jsonMap appContributions)
        , ("surveys", jsonMap appSurveys)
        , ("owners", bsJsonMap appOwners)
        , ("mailbox", mailbox appMailbox)
        , ("key", toJSON appAdminKey)
        ]

        where

        invoiced =
            let handle (k, PaidForDOp _ _) = Nothing
                handle (k, PaidForOrder ix) = Just (toJSON (Base64Encoded k, ix))
            in toJSON $ Maybe.mapMaybe handle $ Map.toList appInvoiced

        mailbox = jsonMap . getCompose . fmap (SessionMessage 0 Nothing) . Compose

        bsJsonMap :: ToJSON v => Map ByteString v -> Value
        bsJsonMap = toJSON . fmap (first Base64Encoded) . Map.toList

        jsonMap :: (ToJSON a, ToJSON b) => Map a b -> Value
        jsonMap = toJSON . Map.toList


instance Ae.FromJSON AppState where
    parseJSON = withObject "AppState" $ \obj ->
        let invoiced = Map.fromList . fmap f <$> obj .: "invoiced"
            f (Base64Encoded k, ix) = (k, PaidForOrder ix)
            -- sessions = Map.fromList . fmap (second $ fmap messageBody)
            bsMap = Map.fromList . fmap (first unBase64Encode)
        in
        AppState <$>
            fmap Map.fromList (obj .: "items") <*>
            fmap Map.fromList (obj .: "orders") <*>
            fmap Map.fromList (obj .: "contributions") <*>
            fmap Map.fromList (obj .: "surveys") <*>
            invoiced <*>
            fmap bsMap (obj .: "blobs") <*>
            fmap bsMap (obj .: "owners") <*>
            pure Map.empty <*>
            -- fmap sessions (obj .: "sessions") <*>
            obj .: "key"


emptyState =
    AppState
        Map.empty -- Items
        Map.empty -- Orders
        Map.empty -- Contributions
        Map.empty -- Surveys
        Map.empty -- Invoiced
        Map.empty -- Blobs
        Map.empty -- Owners
        Map.empty -- Sessions

sha256 = BA.convert . hashWith SHA256

mkItemKey (Key bytes) = Key . sha256  $ bytes <> "/item"
mkBlobId (Key userKey) (Key blobKey) = sha256 $ userKey <> blobKey

-- ~~~~~~~~~~~ --
-- Addressable --
-- ~~~~~~~~~~~ --

-- | Type class for types that admit CRUD-type operations with respect to some
-- backend
class Addressable k a where
    runDataOp :: DataOp k a -> AppState -> AppM (AppST, AppState)

runDataOpMap :: (Applicative m, Ord k)
    => (a -> Maybe k -> k)
    -> DataOp k a
    -> Map k a
    -> m (AppST, Map k a)
runDataOpMap nextId op state = case op of
    -- | If the suggested key is available insert the object with the suggested
    -- key.  Otherwise find an unused key for the object.
    DNew x suggestion k ->
        let ix = fromMaybe newKey $ vet =<< suggestion
            newKey = nextId x $ fst <$> Map.lookupMax state
            vet s = Just s `maybe` const Nothing $ Map.lookup s state
            state' = Map.insert ix x state
        in
        pure (k ix, state')

    DUpdate ix f next ->
        let state' = Map.adjust f ix state in
        pure (next, state')

    DView fx k ->
        let lookup' = flip Map.lookup state in
        pure (k $ lookup' <$> fx, state)

nextW32 _ = maybe 0 (+1)

instance Addressable Word32 Item where
    runDataOp op state@AppState{..} =
        let f x = state { appItems = x }
        in second f <$> runDataOpMap nextW32 op appItems

instance Addressable Word32 Survey where
    runDataOp op state@AppState{..} =
        let f x = state { appSurveys = x }
        in second f <$> runDataOpMap nextW32 op appSurveys

instance Addressable Word32 Order where
    runDataOp op state@AppState{..} =
        let f x = state { appOrders = x }
        in second f <$> runDataOpMap nextW32 op appOrders


instance Addressable Word32 Contribution where
    runDataOp op state@AppState{..} =
        let f x = state { appContributions = x }
        in second f <$> runDataOpMap nextW32 op appContributions


instance Addressable ByteString Blob where
    runDataOp op state@AppState{..} =
        let f x = state { appBlobs = x }
            nextId (Blob b _) _ = sha256 b
        in second f <$> runDataOpMap nextId op appBlobs


-- ~~~ --
-- DSL --
-- ~~~ --

-- | Generic operations on primary data
data DataOp k a where
    -- | We allow a key suggestion
    DNew :: a -> Maybe k -> (k -> AppST) -> DataOp k a

    DUpdate :: k -> (a -> a) -> AppST -> DataOp k a

    DView :: Functor f => f k -> (f (Maybe a) -> AppST) -> DataOp k a


rview' :: Id a -> (Maybe a -> AppST) -> DataOp (Id a) a
rview' x f  = DView (Identity x) (f . runIdentity)


data PaidFor where
    PaidForDOp :: Addressable k a => ByteString -> DataOp k a -> PaidFor
    PaidForOrder :: Id Order -> PaidFor


-- | DSL for performing operations
data AppST where
    NoOp :: AppST

    Emit :: MessageTo Client -> AppST

    ExRate :: Word32 -> (Word32 -> AppST) -> AppST

    WithPR :: MoneyAmount -> Text -> (PaymentRequest -> AppST) -> AppST

    WithCurrentTime :: (UTCTime -> AppST) -> AppST

    RunDataOp :: Addressable k a => DataOp k a -> AppST

    PaymentHandler :: ByteString -> PaidFor -> AppST -> AppST

    WithInvoice :: ByteString -> (Invoice -> AppST) -> AppST

    ResolveInvoiced :: ByteString -> (Maybe TaggedId -> AppST) -> AppST

    RolloverInvoice :: ByteString -> ByteString -> AppST -> AppST

    VerifyCapability :: CapabilityProof -> AppST -> AppST -> AppST



-- ~~~~~~~~~ --
-- App logic --
-- ~~~~~~~~~ --

-- | Compute the resource requirements needed to handle a message
protocol :: Pricing -> MessageTo Server -> AppST
protocol Pricing{..} = \case

    Sync _ -> NoOp

    -- Create a payment request and send it back
    Donate msg amount ->
        WithPR (Satoshis amount) "donation" $ \req@(PaymentRequest hash _) ->
        let newContribution :: DataOp (Id Contribution) Contribution
            newContribution = DNew (Contribution amount msg) Nothing $ \ix ->
                Emit $ Confirmation (IdT ContributionT ix) (Just hash)
        in
        PaymentHandler hash (PaidForDOp hash newContribution) $
        Emit $ PaymentRequestMsg req

    -- Create a payment request for the total amount and send it back
    Purchase stuff key ->
        -- We preserve the association between items and quantities during item
        -- inflation by viewing [(Quantity, Id Item)] as a functor over Id Item
        let stuff' :: Compose [] ((,) Quantity) (Id Item)
            stuff' = Compose $ swap <$> stuff
        in
        RunDataOp $ DView stuff' $ \(Compose items) ->

        let totalCents = List.sum $ adjPrice <$> items
            adjPrice (Quantity q, item') = case item' of
                Nothing   -> 0
                Just item -> itemPrice item * fromIntegral q
        in
        WithPR (Cents totalCents) "order" $ \req@(PaymentRequest hash _) ->
        let order = Order stuff hash key False False in
        RunDataOp $ DNew order Nothing $ \ix ->
            PaymentHandler hash (PaidForOrder ix) $
            Emit $ PaymentRequestMsg req

    NewPR hash ->
        WithInvoice hash $ \Invoice{..} ->
        WithPR (Satoshis invoiceAmount) "new invoice" $ \req@(PaymentRequest hash' _) ->
        RolloverInvoice hash hash' $
        Emit $ PaymentRequestMsg req

    NewItem{..} ->
        let item = Item newItemDescription newItemPrice

            newItem :: DataOp (Id Item) Item
            newItem = DNew item Nothing $ \ix ->
                Emit $ Confirmation (IdT ItemT ix) Nothing

        in
        VerifyCapability (NewItemCap storeKey) (Emit Ack) $
        RunDataOp newItem

    NewSurvey{..} ->
        WithPR priceSurvey "survey" $ \req@(PaymentRequest hash _) ->
        let survey = Survey newSurveyTitle newSurveyQuestions []

            createSurvey :: DataOp (Id Survey) Survey
            createSurvey = DNew survey Nothing $ \ix ->
                Emit $ Confirmation (IdT SurveyT ix) (Just hash)

        in
        PaymentHandler hash (PaidForDOp hash createSurvey) $ Emit $ PaymentRequestMsg req

    NewBlob{..} ->
        if blobLifetime > 365 then NoOp else

        let totalCost (Satoshis x) = Satoshis $ blobLifetime * x
            totalCost (Cents x)    = Cents $ blobLifetime * x
        in
        WithPR (totalCost priceBlobRate) "blob storage" $ \req@(PaymentRequest hash _) ->
        WithCurrentTime $ \now ->

        let storeBlob :: DataOp (Id Blob) Blob
            storeBlob = DNew newBlob (Just blobKey) $ \ix ->
                Emit $ Confirmation (IdH BlobT ix) (Just hash)
            newBlob = Blob blob $
                let UTCTime d dt = now in
                UTCTime (addDays (fromIntegral blobLifetime) d) dt
        in
        PaymentHandler hash (PaidForDOp hash storeBlob) $ Emit $ PaymentRequestMsg req

    SurveyResponse{..} ->
        let dview :: DataOp (Id Survey) Survey
            dview = rview' surveyId $ \case
                Nothing -> Emit Ack
                Just _ ->
                    let update s = s { surveyResponses = surveyResponse : surveyResponses s } in
                    RunDataOp (DUpdate surveyId update $ Emit Ack :: DataOp (Id Survey) Survey)
        in
        RunDataOp dview


    Resolve{..} ->
        let emit :: ToJSON a => Maybe a -> AppST
            emit = Emit . maybe Ack (Object . toJSON . (resolutionId,))
        in
        case resolutionId of
        IdT SurveyT ix ->
            let dview :: DataOp (Id Survey) Survey
                dview = rview' ix emit
            in
            RunDataOp dview

        IdT ItemT ix   ->
            let dview :: DataOp (Id Item) Item
                dview = rview' ix emit
            in
            RunDataOp dview

        IdT OrderT ix  ->
            let dview :: DataOp (Id Order) Order
                dview = rview' ix $
                    let guardKey obj = resolutionKey >>= \key ->
                            obj >>= \o@Order{..} ->
                            if orderKey == key then Just o else Nothing
                    in
                    emit . guardKey
            in
            RunDataOp dview

        IdT ContributionT ix ->
            let dview :: DataOp (Id Contribution) Contribution
                dview = rview' ix emit
            in
            RunDataOp dview

    where

    repackSurvey :: Survey -> Ae.Value
    repackSurvey Survey{..} = _Object . _Wrapped #
        [ ("title", _String # surveyTitle )
        , ("questions", _JSON # surveyQuestions )
        ]


-- | This is what should happen with an invoice hash
sweepOne hash =
    WithInvoice hash $ \Invoice{..} ->
        if  | invoiceSettled && invoiceAmountPaid >= invoiceAmount ->
                ResolveInvoiced hash $ \case
                Nothing -> NoOp
                Just x -> case x of
                    IdH DOpT _ -> NoOp
                    _          -> Emit $ Confirmation x $ Just hash

            -- there was a payment but it is not enough to cover the order
            | invoiceSettled && invoiceAmountPaid < invoiceAmount ->
                let balance = Satoshis $ invoiceAmount - invoiceAmountPaid in
                WithPR balance "rolling over balance" $ \req@(PaymentRequest hash' _) ->
                RolloverInvoice hash hash' $
                Emit $ PaymentRequestMsg req

            | otherwise -> NoOp



-- ~~~~~~~~~ --
-- Evaluator --
-- ~~~~~~~~~ --

-- | Supply resources as required
runApp :: Monoid r
    => ((Key -> StateT AppState AppM r) -> StateT AppState AppM r)
    -> (MessageTo Client -> AppM r)
    -> AppM r
    -> AppST
    -> StateT AppState AppM r
runApp onlyWithKey emit noop = \case
    NoOp -> lift noop

    Emit x ->
        logDebugN "Emitting" >>
        lift (emit x)

    ExRate amountCents k ->
        let x = Text.pack $ show amountCents
            msg = "ExRate " <> x
        in
        logDebugN msg >>
        lift (centsToSatoshis amountCents) >>=
        runApp' . k

    WithPR amount memo k ->
        let msg = "WithPR " <> Text.pack (show amount) <> " " <> Text.pack (show memo) in
        logDebugN msg >>

        lift (toSatoshis amount) >>= \sat ->

        if sat == 0 then lift noop else

        lift (Lnd.newInvoice sat memo) >>=
        runApp' . k

    WithCurrentTime k ->
        liftIO Time.getCurrentTime >>=
        runApp' . k

    PaymentHandler hash thing next -> onlyWithKey $ \key ->
        let logThing = case thing of
                PaidForDOp _ _ -> "resource op"
                PaidForOrder _ -> "order"
            logText = "PaymentHandler " <> logThing
        in
        logDebugN logText >>
        let f state@AppState{..} = state
                { appInvoiced = Map.insert hash thing appInvoiced
                , appOwners = Map.insert hash key appOwners
                }
        in
        modify f >>
        runApp' next

    RunDataOp op ->
        logDebugN "RunDataOp" >>
        StateT (runDataOp op) >>=
        runApp'

    WithInvoice hash k ->
        let logText = Text.pack $ BS8.unpack $
                "WithInvoice " <> B64.encode hash
        in
        logDebugN logText >>
        lift (Lnd.getInvoice hash) >>= \invoice ->
        runApp' (k invoice)


    ResolveInvoiced h k ->
        get >>= \state@AppState{..} ->

        let state' = state { appInvoiced = Map.delete h appInvoiced }
            paidItem = Map.lookup h appInvoiced
            logText = Text.pack $ BS8.unpack $ "ResolveInvoice " <> B64.encode h
        in
        put state' >>
        logDebugN logText >>
        case paidItem of
            Nothing ->
                logErrorN "Item not found" >>
                runApp' (k Nothing)

            Just (PaidForDOp ix op) ->
                logDebugN "Doing the data op" >>
                StateT (runDataOp op) >>= \op' ->
                (<>) <$> runApp' op' <*>
                runApp' (k $ Just $ IdH DOpT ix)

            Just (PaidForOrder ix) ->
                let orderUpdate = RunDataOp $ DUpdate ix update $ k $ Just (IdT OrderT ix)
                    update order = order { orderPaid = True }
                in
                logDebugN "Marking order paid" >>
                runApp' orderUpdate

    RolloverInvoice h h' next ->
        let rollover state@AppState{..} =
                state { appInvoiced = f h h' appInvoiced, appOwners = f h h' appOwners }

            f k k' x = Map.delete k $
                    maybe x (flip (Map.insert k') x) $
                    Map.lookup k x

            logText = Text.pack $ BS8.unpack $
                "RolloverInvoice " <> B64.encode h <> " -> " <> B64.encode h'
        in
        logDebugN logText >>
        modify rollover >>
        runApp' next

    VerifyCapability capProof notOk ok ->
        case capProof of
            NewItemCap key ->
                let f = Text.pack . BS8.unpack . BSL.toStrict . Ae.encode in
                get >>= \AppState{..} ->
                logDebugN (f key) >>
                logDebugN (f appAdminKey) >>
                let result = if key == mkItemKey appAdminKey then ok else notOk in
                runApp' result

    where

    runApp' = runApp onlyWithKey emit noop

    toSatoshis = \case
        Cents x    -> centsToSatoshis x
        Satoshis x -> return x



-- | We can use: https://api.coinmarketcap.com/v1/ticker/?limit=5
-- but it might go offline at any time
--
centsToSatoshis :: Word32 -> AppM Word32
centsToSatoshis cents = return $ floor $ fixedRate * fromIntegral cents
    where

    fixedRate = 1e8 / 373574 -- cents / 1e8 satoshis



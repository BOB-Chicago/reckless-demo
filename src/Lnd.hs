-- |
-- Module: Lnd
--
-- lnd integration via REST api

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lnd (
    lndParams
    , postInvoice
    , newInvoice
    , getInvoices
    , getInvoice
    ) where

import           Control.Arrow
import           Control.Exception
import           Control.Lens               as Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Hash                as Crypto
import           Crypto.Random              as Crypto
import           Data.Aeson                 as Ae
import           Data.Aeson.Lens
import           Data.Bifunctor             as Bf
import           Data.ByteArray             as BA
import           Data.ByteString            as BS
import           Data.ByteString.Base16     as B16
import           Data.ByteString.Base64     as B64
import           Data.ByteString.Char8      as BS8
import           Data.Default
import           Data.Maybe
import           Data.Text                  as Text
import           Data.Text.Encoding         as TE
import           Data.Vector                as Vector
import           Data.Word
import           Data.X509.CertificateStore
import           Network.Connection
import           Network.HTTP.Client        as Http
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types         as HttpT
import           Network.TLS                as Tls
import           Network.TLS.Extra.Cipher   as Ex

import           Types


-- | Setup an LND session
lndParams ::
    FilePath
    -- ^ path to certificate
    -> FilePath
    -- ^ path to macaroon
    -> Int
    -- ^ RPC port
    -> ByteString
    -- ^ RPC host
    -> ExceptT AppError IO (Config, Manager)
lndParams certPath macaroonPath port host = do
  certStore <- loadCert certPath
  macaroonRaw <- liftIO $ BS.readFile macaroonPath
  mgr <- newTlsManagerWith $ mkManagerSettings (lndTlsSettings certStore) Nothing
  return (Config host port macaroonRaw, mgr)

  where

  loadCert = ExceptT . fmap (maybe (Left LoadCertError) Right) . liftIO . readCertificateStore


-- | TLS settings where the certificate store has the LND certificate
lndTlsSettings lndCAStore = TLSSettings $ ClientParams
  { clientUseMaxFragmentLength = Nothing
  , clientServerIdentification = ("localhost", "")
  , clientUseServerNameIndication = False
  , clientWantSessionResume = Nothing
  , clientShared = def { sharedCAStore = lndCAStore }
  , clientHooks = def
  , clientSupported = def { supportedCiphers = Ex.ciphersuite_default }
  , clientDebug = def
  }

-- | Query LND
lndCall :: FromJSON a => Request -> AppM a
lndCall req = preliminaryCall >>= interpretResponse

    where

    interpretResponse = ExceptT . return . Bf.first DecodingError . Ae.eitherDecode . responseBody

    preliminaryCall = ExceptT $ ReaderT $ \(Config{..}, mgr) ->
        let fullReq = req
                { host = lndHost
                , port = lndPort
                , secure = True
                , requestHeaders =
                [ (hContentType, "application/json")
                , (hAccept, "application/json")
                , ("Grpc-metadata-macaroon", B16.encode macaroon)
                ]
                }
        in liftIO $ Bf.first AppHttpException <$> try (httpLbs fullReq mgr)


-- ~~~~~~~~~ --
-- Endpoints --
-- ~~~~~~~~~ --


-- | POST /v1/invoices
postInvoice :: InvoiceRequest -> AppM PaymentRequest
postInvoice InvoiceRequest{..} =
    lndCall req >>= \v ->
        let (hash', pr) = parse v in
        PaymentRequest <$> maybe (throwError $ DecodingError "r_hash") return hash' <*> pure pr

    where

    req = defaultRequest
        { method = "POST"
        , path = "/v1/invoices"
        , requestBody = RequestBodyLBS payload
        }

    rHashPreimage = TE.decodeUtf8 $ B64.encode invoicePreimage
    payload = _Object . _Wrapped #
        [ ("r_preimage", _String # rHashPreimage)
        , ("value", _Integral # invoiceRequestAmount)
        , ("memo", _String # invoiceMemo)
        ]

    parse :: Value -> (Maybe ByteString, Text)
    parse = fmap unBase64Encode . Lens.preview (key "r_hash" . _JSON) &&&
            Lens.view (key "payment_request" . _String)



-- | GET /v1/invoices
getInvoices ::
    Bool
    -- ^ Only include pending invoices
    -> Maybe (Word32, Word32)
    -- ^ Pagination parameters
    -> AppM [Invoice]
getInvoices onlyPending pagination =
  parse <$> lndCall (defaultRequest { path = "/v1/invoices", queryString = qs })

  where

    qs = renderQuery False $ catMaybes
        [ if onlyPending then Just ("pending_only", Just "true") else Nothing
        , mkParam "index_offset" fst <$> pagination
        , mkParam "num_max_invoices" snd <$> pagination
        ]

    mkParam name getVal x = let val = BS8.pack $ show $ getVal x in (name, Just val)

    parse :: Value -> [Invoice]
    parse = Lens.view (key "invoices" . _JSON)


-- | GET /v1/invoice/${r_hash}
getInvoice :: ByteString -> AppM Invoice
getInvoice rHash =
  lndCall $ defaultRequest { path = "/v1/invoice/" <> B16.encode rHash }


-- ~~~~~~~ --
-- Helpers --
-- ~~~~~~~ --


newInvoice :: Word32 -> Text -> AppM PaymentRequest
newInvoice amount memo =
    liftIO (Crypto.getRandomBytes 32) >>= \preimage ->
        postInvoice $ InvoiceRequest preimage amount memo


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where


import           Control.Monad.Except
import           Control.Monad.Logger   as Log
import           Control.Monad.Reader
import           Data.Aeson             as Ae
import           Data.ByteString        as BS
import           Data.ByteString.Base16 as B16
import           Data.ByteString.Base64 as B64
import           Data.ByteString.Char8  as BS8
import           Data.List              as List
import           Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Text              as Text
import           Data.Text.Encoding     as TE
import           Data.Time              as Time
import           Data.Vector            as Vector
import           Data.Word
import           GHC.Generics
import qualified Network.HTTP.Client    as Http



-- ~~~~~~~~~ --
-- App types --
-- ~~~~~~~~~ --


-- | Core monad transformer stack
type AppM = ExceptT AppError (ReaderT (Config, Http.Manager) (LoggingT IO))


-- | Application configuration
data Config
    = Config
    { lndHost  :: ByteString
    , lndPort  :: Int
    , macaroon :: ByteString
    } deriving (Eq, Show)


-- | Error states
data AppError
    = DecodingError String
    | LoadCertError
    deriving Show


newtype CapabilityProof
    = NewItemCap Key


data Pricing
    = Pricing { priceSurvey :: MoneyAmount, priceBlobRate :: MoneyAmount }

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Primitive types & newtypes --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~ --


-- | Supported currencies
data MoneyAmount
    = Cents Word32
    | Satoshis Word32
    deriving (Eq, Show)


newtype Quantity = Quantity Word16
    deriving (Eq, Ord, ToJSON, FromJSON)


-- | ByteStrings that should be (de)serialized to hex strings
newtype Base64Encoded = Base64Encoded { unBase64Encode :: ByteString }

instance FromJSON Base64Encoded where
    parseJSON = withText "Base64Encoded" $
        either fail (return . Base64Encoded) . B64.decode . TE.encodeUtf8

instance ToJSON Base64Encoded where
    toJSON = toJSON . BS8.unpack . B64.encode . unBase64Encode

-- | ByteStrings that should be (de)serialized to hex strings
newtype HexEncoded = HexEncoded { unHexEncode :: ByteString }

instance FromJSON HexEncoded where
    parseJSON = withText "HexEncoded" $
        return . HexEncoded . fst . B16.decode . TE.encodeUtf8

instance ToJSON HexEncoded where
    toJSON = toJSON . BS8.unpack . B16.encode . unHexEncode

-- ~~~~~~~~~~~~~~ --
-- App data model --
-- ~~~~~~~~~~~~~~ --


data Survey
    = Survey
    { surveyTitle     :: Text
    , surveyQuestions :: Vector Text
    , surveyResponses :: [Vector Text]
    } deriving (Eq, Show, Generic)

instance ToJSON Survey
instance FromJSON Survey


data Item
    = Item
    { itemDescription :: Text
    , itemPrice       :: Word32
    } deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item


type NewOrder = [(Id Item, Quantity)]


data Order where
    Order :: { orderDetails :: [(Id Item, Quantity)]
             , orderInvoiceHash :: ByteString
             , orderKey :: Key
             , orderPaid :: Bool
             , orderFulfilled :: Bool
             } -> Order

instance ToJSON Order where
    toJSON Order{..} = object
        [ ("details", toJSON orderDetails)
        , ("invoiceHash", toJSON (Base64Encoded orderInvoiceHash))
        , ("key", toJSON orderKey)
        , ("paid", toJSON orderPaid)
        , ("fulfilled", toJSON orderFulfilled)
        ]

instance FromJSON Order where
    parseJSON = withObject "Order" $ \obj ->
        Order <$> obj .: "details" <*>
            fmap unBase64Encode (obj .: "invoiceHash") <*>
            obj .: "key" <*>
            obj .: "paid" <*>
            obj .: "fulfilled"


data Contribution = Contribution Word32 Text
    deriving (Show, Generic)

instance ToJSON Contribution
instance FromJSON Contribution

data Blob = Blob { blobBytes :: ByteString, blobExpires :: UTCTime }

instance ToJSON Blob where
    toJSON Blob{..} = toJSON (Base64Encoded blobBytes, blobExpires)

instance FromJSON Blob where
    parseJSON =
        let f (Base64Encoded b, t) = Blob b t in
        fmap f . parseJSON

type family Id a where
    Id Order = Word32
    Id Survey = Word32
    Id Contribution = Word32
    Id Item = Word32

    Id Blob = ByteString


data TagW32 = ItemT | SurveyT | OrderT | ContributionT
    deriving (Eq, Show, Generic)

instance ToJSON TagW32 where
    toJSON = toJSON . show

data TagBS = DOpT | BlobT
    deriving (Eq, Show, Generic)

instance ToJSON TagBS where
    toJSON = toJSON . show

data TaggedId
    = IdT TagW32 Word32
    | IdH TagBS ByteString
    deriving (Eq, Show, Generic)

instance ToJSON TaggedId where
    toJSON (IdT t x) = object [ ("type", toJSON t), ("value", toJSON x) ]
    toJSON (IdH t h) = object [ ("type", toJSON t), ("value", toJSON (Base64Encoded h)) ]



newtype Key = Key ByteString
    deriving (Eq, Show, Ord)

instance FromJSON Key where
    parseJSON = fmap (Key . fst . B16.decode . BS8.pack) . parseJSON

instance ToJSON Key where
    toJSON (Key x) = toJSON $ BS8.unpack $ B16.encode x

type Page a = [(Id a, a)]


-- ~~~~~~~~ --
-- Protocol --
-- ~~~~~~~~ --


data Participant = Client | Server


-- | UI protocol
data MessageTo a where
    Ack :: MessageTo Client

    -- | The server requests payment
    PaymentRequestMsg :: PaymentRequest -> MessageTo Client

    Confirmation :: TaggedId -> Maybe ByteString -> MessageTo Client

    -- | The server sends the client its order history
    OrderHistory :: Page Order -> MessageTo Client

    -- | The server advertises the open surveys
    OpenSurveys :: Page Survey -> MessageTo Client

    -- | The server can send any object to the client
    Object :: Value -> MessageTo Client

    -- | Used to establish an configure the session
    Sync :: Key -> MessageTo Server

    -- | The client initiates a donation
    Donate :: Text -> Word32 -> MessageTo Server

    -- | The client submits an order
    Purchase :: NewOrder -> Key -> MessageTo Server

    -- | Request a new PR
    NewPR :: ByteString -> MessageTo Server

    -- | Create an item
    NewItem :: { storeKey :: Key
               , newItemDescription :: Text
               , newItemPrice :: Word32
               } -> MessageTo Server

    -- | Create a new survey, supplying a key to be used for administration
    NewSurvey :: { surveyKey :: ByteString
                 , newSurveyTitle :: Text
                 , newSurveyQuestions :: Vector Text } -> MessageTo Server

    -- | Upload a new blob
    NewBlob :: { blob :: ByteString, blobKey :: ByteString, blobLifetime :: Word32 } -> MessageTo Server

    -- | The client responds to a survey
    SurveyResponse :: { surveyId :: Id Survey, surveyResponse :: Vector Text } -> MessageTo Server

    -- | A client can ask the server to resolve any identifier that it knows about
    Resolve :: { resolutionId :: TaggedId, resolutionKey :: Maybe Key } -> MessageTo Server


data SessionMessage a
    = SessionMessage
    { messageNonce :: Word32
    , nonceRef     :: Maybe Word32
    , messageBody  :: MessageTo a
    }


instance ToJSON (SessionMessage Client) where

    toJSON (SessionMessage n ref msg) =
        let message :: Text -> [(Text, Ae.Value)] -> Value
            message t xs = object $
                let fields = ("tag", toJSON t) : ("nonce", toJSON n) : xs in
                maybe fields (\r -> ("ref", toJSON r):fields) ref

        in case msg of
            Ack -> message "ack" []

            PaymentRequestMsg (PaymentRequest hash req) ->
                message "paymentRequest"
                    [ ("paymentRequest", toJSON req)
                    , ("rHash", toJSON (Base64Encoded hash)) ]

            Confirmation tix h ->
                message "confirmation"
                    [ ("id", toJSON tix)
                    , ("rRash", toJSON (Base64Encoded <$> h))
                    ]

            OrderHistory xs ->
                message "orderHistory" [("orders", toJSON xs)]

            OpenSurveys xs ->
                message "openSurveys" [("surveys", toJSON xs)]

            Types.Object x -> message "object" [("payload", toJSON x)]


instance FromJSON (SessionMessage Server) where
    parseJSON = withObject "SessionMessage" $ \obj ->
        obj .: "nonce" >>= \n ->
        obj .:? "ref" >>= \r ->
        obj .: "tag" >>= \(tag :: Text) -> fmap (SessionMessage n r) $ if
            | tag == "sync" -> Sync <$> obj .: "key"

            | tag == "donate" -> Donate <$> obj .: "message" <*> obj .: "amount"

            | tag == "purchase" -> Purchase <$> obj .: "stuff" <*> obj .: "key"

            | tag == "newPR" -> NewPR . unBase64Encode <$> obj .: "rHash"

            | tag == "newItem" ->
                NewItem <$> obj .: "key" <*> obj .: "description" <*> obj .: "price"

            | tag == "newSurvey" ->
                let key = unBase64Encode <$> obj .: "key" in
                NewSurvey <$> key <*> obj .: "title" <*> obj .: "questions"

            | tag == "newBlob" ->
                NewBlob <$>
                (unHexEncode <$> obj .: "data") <*>
                (unHexEncode <$> obj .: "key") <*>
                obj .: "lifetime"

            | tag == "surveyResponse" -> SurveyResponse <$> obj .: "id" <*> obj .: "responses"

            | tag == "resolve" ->
                let key = fmap (Key . unBase64Encode) <$> obj .:? "key" in
                obj .: "id" >>= \i ->
                obj .: "type" >>= \(t :: Text) ->
                    if | t == "item" -> Resolve (IdT ItemT i) <$> key
                       | t == "survey" -> Resolve (IdT SurveyT i) <$> key
                       | t == "order" -> Resolve (IdT OrderT i) <$> key
                       | t == "contribution" -> Resolve (IdT ContributionT i) <$> key
                       | otherwise -> fail "Unknown type"

            | otherwise -> fail "Unknown tag"


-- ~~~~~~~~~~~~~~ --
-- Lighting stuff --
-- ~~~~~~~~~~~~~~ --

-- | Simplified invoice model
data Invoice
    = Invoice
    { invoiceHash       :: ByteString
    , invoiceAmount     :: Word32
    , invoiceAmountPaid :: Word32
    , invoiceSettled    :: Bool
    } deriving (Eq, Show)


instance FromJSON Invoice where
    parseJSON = withObject "Invoice" $ \obj ->
        let hash = unBase64Encode <$> obj .: "r_hash"

            settled = f <$> obj .:? "settled"
            f = fromMaybe False

            value = read <$> obj .: "value"

            paid = maybe 0 read <$> obj .:? "amt_paid_sat"
        in
        Invoice <$> hash <*> value <*> paid <*> settled

instance ToJSON Invoice where
    toJSON Invoice{..} = object
        [ ("r_hash", toJSON (Base64Encoded invoiceHash))
        , ("amount", toJSON invoiceAmount)
        , ("amount_paid", toJSON invoiceAmountPaid)
        , ("settled", toJSON invoiceSettled)
        ]


-- | Simplified invoice request model
data InvoiceRequest
    = InvoiceRequest
    { invoicePreimage      :: ByteString
    , invoiceRequestAmount :: Word32
    , invoiceMemo          :: Text
    } deriving (Eq, Show)

-- | A BOLT 11 payment request and associated hash
data PaymentRequest = PaymentRequest ByteString Text
    deriving (Eq, Show)


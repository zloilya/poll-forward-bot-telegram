module Api where

import Data.Aeson.Types
  (FromJSON(parseJSON), GFromJSON, GToJSON,
  Options(constructorTagModifier, fieldLabelModifier, omitNothingFields, sumEncoding), Parser,
  SumEncoding(TaggedObject, contentsFieldName, tagFieldName), ToJSON(toJSON), Value(String), Zero,
  defaultOptions, genericParseJSON, genericToJSON)
import qualified Data.Char as Char
import Data.Int (Int64)
import Data.List (drop, isPrefixOf)
import Data.Text (Text, unpack)
import GHC.Generics (Generic(Rep))

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a.(Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix = genericToJSON defaultOptions {
    fieldLabelModifier = drop prefix
  , omitNothingFields = True
  }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a.(Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions { fieldLabelModifier = drop prefix }

data Poll = Poll {
    poll_id       :: String
  , poll_question :: Text
} deriving (Show, Generic)

instance ToJSON Poll where
  toJSON = toJsonDrop 5

instance FromJSON Poll where
  parseJSON = parseJsonDrop 5

-- | This object represents a chat.
data Chat = Chat
  { chat_id                             :: Int64
  , chat_username                       :: Maybe Text -- ^ Username, for private chats and channels if available
  } deriving (Show, Generic)

instance ToJSON Chat where
  toJSON = toJsonDrop 5

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

-- | This object represents an incoming update.
-- Only one of the optional parameters can be present in any given update.
data Update = Update
  {
    update_id            :: Int   -- ^ The update's unique identifier. Update identifiers start from a certain positive number and increase sequentially. This ID becomes especially handy if you’re using 'setWebhooks', since it allows you to ignore repeated updates or to restore the correct update sequence, should they get out of order.
  , message              :: Maybe Message -- ^ New incoming message of any kind — text, photo, sticker, etc.
  } deriving (FromJSON, ToJSON, Show, Generic)

-- | This object represents a message.
data Message = Message
  {
    message_id              :: Int -- ^ Unique message identifier
  , chat                    :: Chat -- ^ Conversation the message belongs to
  , poll                    :: Maybe Poll
  } deriving (FromJSON, ToJSON, Show, Generic)


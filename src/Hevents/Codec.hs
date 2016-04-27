module Hevents.Codec where

import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Char              (isHexDigit)
import           Data.List              as List
import           Data.String
import           Data.Text              as T
import qualified Data.Text.Encoding     as LE


data Base64
data Hex

newtype Encoded code = Encoded { encodedText :: Text } deriving (Eq, Ord)

instance Show (Encoded s) where
  show (Encoded t) = show t

instance Read (Encoded s) where
  readsPrec n = List.map ( \ (t,s) -> (Encoded t, s)) . readsPrec n

instance ToJSON (Encoded Hex) where
  toJSON (Encoded t) = String t

instance FromJSON (Encoded Hex) where
  parseJSON (String t) = if T.all isHexDigit t
                         then return $ Encoded t
                         else fail $ "not a valid hexadecimal encoded string: " ++ show t
  parseJSON v          = fail $ "not a valid hexadecimal encoded string: "  ++ show v

instance IsString (Encoded Hex) where
  -- not quite correct
  fromString = Encoded . T.pack

toBase64Text :: ByteString -> Encoded Base64
toBase64Text = Encoded . LE.decodeUtf8 . B64.encode

fromBase64Text :: Encoded Base64 -> ByteString
fromBase64Text = B64.decodeLenient . LE.encodeUtf8 . encodedText

encodeBase64 :: ByteString -> ByteString
encodeBase64 = B64.encode

module Basics.Basics (toBase64) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B

toBase64 :: B.ByteString -> B.ByteString
toBase64 = B64.encode . fst . B16.decode

module Lib
    ( hexDecode
    , hexEncode
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

hexDecode :: B.ByteString -> B.ByteString
hexDecode = fst . B16.decode

hexEncode :: B.ByteString -> B.ByteString
hexEncode = B16.encode

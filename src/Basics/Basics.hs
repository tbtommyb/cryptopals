module Basics.Basics
  ( hexToBase64
  , fixedXOR
  ) where

import Data.Bits (xor)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import Lib (hexDecode, hexEncode)

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = B64.encode . hexDecode

fixedXOR :: B.ByteString -> B.ByteString -> B.ByteString
fixedXOR key input =
  hexEncode $ B.pack $ B.zipWith xor (hexDecode key) (hexDecode input)

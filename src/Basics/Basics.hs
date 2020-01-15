module Basics.Basics
  ( hexToBase64
  , fixedXOR
  , findCharKey
  , decodeCharKey
  ) where

import Data.List ( sortOn )
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Lib ( hexDecode, hexEncode, xorPair, chiSquared )

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = B64.encode . hexDecode

fixedXOR :: B.ByteString -> B.ByteString -> B.ByteString
fixedXOR key input =
  hexEncode $ xorPair (hexDecode key) (hexDecode input)

findCharKey :: B.ByteString -> Char
findCharKey input =
  fst. head . sortOn snd . map (\key -> (key, testKey key input)) $ enumFromTo 'A' 'Z'
  where
    testKey key text = chiSquared $ decodeCharKey key text

decodeCharKey :: Char -> B.ByteString -> B.ByteString
decodeCharKey key input = xorPair (B.replicate (B.length input) key) (hexDecode input)

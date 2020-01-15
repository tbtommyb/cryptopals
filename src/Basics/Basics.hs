module Basics.Basics
  ( hexToBase64
  , fixedXOR
  , findSingleCharXOR
  , checkChar
  ) where

import Data.List ( sortOn )
import Data.Bits ( xor )
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import Lib ( hexDecode, hexEncode, chiSquared, decodeSingleCharXOR )

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = B64.encode . hexDecode

fixedXOR :: B.ByteString -> B.ByteString -> B.ByteString
fixedXOR key input =
  hexEncode $ B.pack $ B.zipWith xor (hexDecode key) (hexDecode input)

findSingleCharXOR :: B.ByteString -> Char
findSingleCharXOR input =
  fst. head . sortOn snd . map (\c -> (c, testKey input c)) $ enumFromTo 'A' 'Z'
  where
    testKey text c = chiSquared $ decodeSingleCharXOR text c


checkChar :: B.ByteString -> Char -> B.ByteString
checkChar = decodeSingleCharXOR

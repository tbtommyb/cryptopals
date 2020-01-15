module Lib
    ( hexDecode
    , hexEncode
    , chiSquared
    , decodeSingleCharXOR
    ) where

import Data.Function ( on )
import Data.Bits ( xor )
import Data.Char ( isLetter, toUpper, ord, chr )
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B

hexDecode :: B.ByteString -> B.ByteString
hexDecode = fst . B16.decode

hexEncode :: B.ByteString -> B.ByteString
hexEncode = B16.encode

englishFrequencies :: [Float]
englishFrequencies = [
  0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228,
  0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025,
  0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987,
  0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150,
  0.01974, 0.00074]

chiSquared :: B.ByteString -> Float
chiSquared input =
  foldr (\char t -> t + computeChi cleanLen char) 0.0 $ getCharCount cleaned
  where
    cleaned = B.filter isLetter $ B.map toUpper input
    cleanLen = B.length cleaned

computeChi :: Int -> (Char, Int) -> Float
computeChi len (char, observed) =
  (fromIntegral observed - expected)**2 / expected
  where
    expected = fromIntegral len * (englishFrequencies !! (ord char - 65))

getCharCount :: B.ByteString -> [(Char, Int)]
getCharCount = map (\x -> (B.head x, B.length x)) . B.group . B.sort

decodeSingleCharXOR :: B.ByteString -> Char -> B.ByteString
decodeSingleCharXOR input key =
  B.pack $ B.zipWith (fmap chr . xor `on` ord) (hexDecode input) (B.replicate (B.length input) key)

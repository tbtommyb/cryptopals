module Lib
    ( hexDecode
    , hexEncode
    , xorPair
    , chiSquared
    , readLines
    , searchForKey
    , findBest
    , decode
    , hammingWeight
    , DecodeAttempt(..)
    , Base64(..)
    , Base16(..)
    ) where

import Data.Bits ( popCount, xor )
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B
import Data.Char ( toUpper, ord, chr )
import Data.Function ( on )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import qualified Data.HashMap as Map

data DecodeAttempt = DecodeAttempt
  { attemptKey :: Char
  , attemptRating :: Float
  , attemptOutput :: B.ByteString
  } deriving (Show)

newtype Base64 = Base64 B.ByteString deriving (Show, Eq)
newtype Base16 = Base16 B.ByteString deriving (Show, Eq)

hexDecode :: Base16 -> B.ByteString
hexDecode (Base16 s) = fst $ B16.decode s

hexEncode :: B.ByteString -> Base16
hexEncode = Base16 . B16.encode

englishFrequencies :: Map.Map Char Float
englishFrequencies = Map.fromList [
  (' ', 0.140), ('E', 0.120), ('T', 0.090), ('A', 0.080), ('O', 0.070), ('I', 0.060),
  ('N', 0.060), ('S', 0.060), ('H', 0.060), ('R', 0.050), ('D', 0.040), ('L', 0.040),
  ('C', 0.020), ('U', 0.020), ('M', 0.020), ('W', 0.020), ('F', 0.020), ('G', 0.020),
  ('Y', 0.010), ('P', 0.010), ('B', 0.010), ('V', 0.010), ('K', 0.010), ('J', 0.010),
  ('X', 0.000), ('Q', 0.000), ('Z', 0.000)]

chiSquared :: B.ByteString -> Float
chiSquared input =
  foldr sumChi 0.0 $ getCharCount $ B.map toUpper input
  where
    sumChi charCount total = total + computeChi charCount

computeChi :: (Char, Float) -> Float
computeChi (c, observed) =
  if expected == 0.0 then 0.0 else ((observed - expected)**2) / expected
  where
    expected = Map.findWithDefault (-1.0) c englishFrequencies

getCharCount :: B.ByteString -> [(Char, Float)]
getCharCount s = map (\xs -> (B.head xs, normalisedFrequency xs)) . B.group $ B.sort s
  where
    normalisedFrequency cx = fromIntegral (B.length cx) / fromIntegral (B.length s)

xorPair :: B.ByteString -> B.ByteString -> B.ByteString
xorPair a b = B.pack $ B.zipWith (fmap chr . xor `on` ord) a b

searchForKey :: B.ByteString -> DecodeAttempt
searchForKey input =
  findBest . map (\key -> testKey key (decode key input)) $ map chr $ enumFromTo 0 255
  where
    testKey key decoded = DecodeAttempt key (chiSquared decoded) decoded

findBest :: [DecodeAttempt] -> DecodeAttempt
findBest = head . sortBy (flip $ comparing attemptRating)

decode :: Char -> B.ByteString -> B.ByteString
decode key input = xorPair (B.replicate (B.length input) key) input

readLines :: FilePath -> IO [Base16]
readLines = fmap ((map Base16) . B.lines) . B.readFile

hammingWeight :: Char -> Char -> Int
hammingWeight = fmap popCount . xor `on` ord

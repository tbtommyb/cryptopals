-- TODO: create Key, EncryptedText, DecryptedText types
module Basics.Basics
  ( hexToBase64
  , fixedXOR
  , findCharKey
  , decodeCharKey
  , findXORLine
  , repeatingXOR
  , hammingDistance
  , decodeRepeatingXOR
  , decodeECB
  , detectECB
  , Base64(..)
  , Base16(..)
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Crypto.Cipher
import Lib
  ( hexDecode
  , hexEncode
  , xorPair
  , searchForKey
  , decode
  , findBest
  , hammingWeight
  , guessKey
  , initAES128
  , duplicateChunks
  , DecodeAttempt(..)
  , Base16(..)
  , Base64(..)
  )

hexToBase64 :: Base16 -> Base64
hexToBase64 = Base64 . B64.encode . hexDecode

fixedXOR :: Base16 -> Base16 -> Base16
fixedXOR key input =
  hexEncode $ xorPair (hexDecode key) (hexDecode input)

findCharKey :: Base16 -> Char
findCharKey = attemptKey . searchForKey . hexDecode

decodeCharKey :: Char -> Base16 -> B.ByteString
decodeCharKey key text = decode key $ hexDecode text

findXORLine :: [B.ByteString] -> B.ByteString
findXORLine xs =
  attemptOutput $ findBest $ map searchForKey xs

repeatingXOR :: B.ByteString -> B.ByteString -> Base16
repeatingXOR key text = hexEncode $ xorPair repeatedKey text
  where
    repeatedKey = B.pack . take (B.length text) . cycle . B.unpack $ key

repeating :: B.ByteString -> B.ByteString -> B.ByteString
repeating key text = xorPair repeatedKey text
  where
    repeatedKey = B.pack . take (B.length text) . cycle . B.unpack $ key

hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance a b = sum $ B.zipWith hammingWeight a b

decodeRepeatingXOR :: B.ByteString -> (B.ByteString, B.ByteString)
decodeRepeatingXOR input =
  (key, repeating key input) where key = guessKey input

decodeECB :: B.ByteString -> B.ByteString -> B.ByteString
decodeECB key text = ecbDecrypt (initAES128 key) text

detectECB :: [Base16] -> Int
detectECB l =  fst $ head $ sortBy (flip $ comparing snd) $ zip [0 .. length l] $ map duplicateChunks $ map hexDecode l

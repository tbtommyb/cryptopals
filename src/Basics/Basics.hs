-- TODO: create Key, EncryptedText, DecryptedText types
module Basics.Basics
  ( hexToBase64
  , fixedXOR
  , findCharKey
  , decodeCharKey
  , findXORLine
  , repeatingXOR
  , Base64(..)
  , Base16(..)
  ) where

import Control.Monad ( liftM )
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Lib
  ( hexDecode
  , hexEncode
  , xorPair
  , readLines
  , searchForKey
  , decode
  , findBest
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

findXORLine :: FilePath -> IO (B.ByteString)
findXORLine path = do
  attempts <- liftM (map $ searchForKey . hexDecode) $ readLines path
  return . attemptOutput . findBest $ attempts

repeatingXOR :: B.ByteString -> B.ByteString -> Base16
repeatingXOR key text = hexEncode $ xorPair repeatedKey text
  where
    repeatedKey = B.pack . take (B.length text) . cycle . B.unpack $ key

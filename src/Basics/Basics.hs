module Basics.Basics
  ( hexToBase64
  , fixedXOR
  , findCharKey
  , decodeCharKey
  , findXorLine
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

findXorLine :: FilePath -> IO (B.ByteString)
findXorLine path = do
  attempts <- liftM (map $ searchForKey . hexDecode) $ readLines path
  return . attemptOutput . findBest $ attempts

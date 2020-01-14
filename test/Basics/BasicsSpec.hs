module Basics.BasicsSpec (spec) where

import qualified Data.ByteString.UTF8 as BSU
import Test.Hspec
import Basics.Basics

spec :: Spec
spec = do
  describe "toBase64" $ do
    it "generates the correct base64 value" $ do
      toBase64 (BSU.fromString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") `shouldBe` BSU.fromString "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

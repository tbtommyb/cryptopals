{-# LANGUAGE OverloadedStrings #-}

module Basics.BasicsSpec (spec) where

import Test.Hspec
import Basics.Basics

spec :: Spec
spec = do
  describe "toBase64" $ do
    it "generates the correct base64 value" $ do
      hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  describe "fixedXOR" $ do
    it "generates the correct output" $ do
      fixedXOR "686974207468652062756c6c277320657965" "1c0111001f010100061a024b53535009181c" `shouldBe` "746865206b696420646f6e277420706c6179"

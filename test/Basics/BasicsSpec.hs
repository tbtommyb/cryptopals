{-# LANGUAGE OverloadedStrings #-}

module Basics.BasicsSpec (spec) where

import Test.Hspec
import Basics.Basics

spec :: Spec
spec = do
  describe "S1C1: hex to base64" $ do
    it "generates the correct base64 value" $ do
      hexToBase64 (Base16 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") `shouldBe` (Base64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  describe "S1C2: fixed-length XOR" $ do
    it "generates the correct output" $ do
      fixedXOR (Base16 "686974207468652062756c6c277320657965") (Base16 "1c0111001f010100061a024b53535009181c") `shouldBe` (Base16 "746865206b696420646f6e277420706c6179")
  describe "S1C3: find the key character" $ do
    it "finds the correct character" $ do
      findCharKey (Base16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
      `shouldBe` 'X'
    it "generates the correct output" $ do
      decodeCharKey 'X' (Base16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") `shouldBe` "Cooking MC's like a pound of bacon"
  describe "S1C4: finding the single-character XOR line" $ do
    it "finds the correct line" $ do
      bestOutput <- findXorLine "test/Basics/4.txt"
      bestOutput `shouldBe` "Now that the party is jumping\n"

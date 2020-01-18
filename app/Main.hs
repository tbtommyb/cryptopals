module Main where

import Basics.Basics

main :: IO ()
main = findXORLine "test/Basics/4.txt" >>= putStrLn . show

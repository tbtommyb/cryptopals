module Main where

import Basics.Basics

main :: IO ()
main = findXorLine "test/Basics/4.txt" >>= putStrLn . show

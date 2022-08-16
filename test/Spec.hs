{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Range
import Test.HUnit
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain $ hUnitTestToTests $ TestList [
  "integer range contains" ~: do
    (r, candidate, expected) <-
      [
        ((Closed 2, Open 6), [2,4], True)
      ]
    let actual = r `contains` candidate
    return $ expected ~=? actual
  ]
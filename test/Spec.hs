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
        ((Closed   2 ,   Open  6), [2,4], True),
        ((Closed   2 ,   Open  6), [-1,1,6,10], False),
        ((Closed (-1), Closed 10), [-1,1,6,10],  True),
        ((Closed (-1),   Open 10), [-1,1,6,10], False)
      ]
    let actual = r `contains` candidate
    return $ expected ~=? actual
  ]
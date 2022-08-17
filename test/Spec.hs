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
        ((Closed   2 ,   Open  6),       [2,4],  True),
        ((Closed   2 ,   Open  6), [-1,1,6,10], False),
        ((Closed (-1), Closed 10), [-1,1,6,10],  True),
        ((Closed (-1),   Open 10), [-1,1,6,10], False),
        ((Closed (-1),   Open 10),  [-1,1,6,9],  True),
        ((  Open   2,  Closed  6),     [3,5,6],  True),
        ((  Open   2,    Open  6),       [2,5], False),
        ((  Open   2,    Open  6),          [],  True),
        ((Closed   2,  Closed  6),     [3,7,4], False)
      ]
    let actual = r `contains` candidate
    return $ expected ~=? actual
  ,
  "getAllPoints" ~: do
    (r, expected) <-
      [
        ((Closed 2,   Open 6), [2..5]),
        ((Closed 4,   Open 8), [4..7]),
        ((Closed 2, Closed 6), [2..6]),
        ((Closed 4, Closed 8), [4..8]),
        ((  Open 2, Closed 6), [3..6]),
        ((  Open 4, Closed 8), [5..8]),
        ((  Open 2,   Open 6), [3..5])
      ]
    let actual = allPoints r
    return $ expected ~=? actual
  ]
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
        ((  Open 2,   Open 6), [3..5]),
        ((  Open 4,   Open 8), [5..7])
      ]
    let actual = allPoints r
    return $ expected ~=? actual
  ,
  "ContainsRange" ~: do
    (r, candidate, expected) <-
      [
        ((Closed 2,   Open  5), allPoints (Closed 7, Open   10), False),
        ((Closed 2,   Open  5), allPoints (Closed 3, Open   10), False),
        ((Closed 3,   Open  5), allPoints (Closed 2, Open   10), False),
        ((Closed 2,   Open 10), allPoints (Closed 3, Closed  5),  True),
        ((Closed 3, Closed  5), allPoints (Closed 3, Open    5),  True)
      ]
    let actual = r `contains` candidate
    return $ expected ~=? actual
  ,
  "endPoints" ~: do
    (r, expected) <-
      [
        ((Closed 2,   Open 6), (2, 5)),
        ((Closed 1,   Open 7), (1, 6)),
        ((Closed 2, Closed 6), (2, 6)),
        ((Closed 1, Closed 7), (1, 7)),
        ((  Open 2,   Open 6), (3, 5)),
        ((  Open 1,   Open 7), (2, 6)),
        ((  Open 2, Closed 6), (3, 6)),
        ((  Open 1, Closed 7), (2, 7))
      ]
    let actual = endpoints r
    return $ expected ~=? actual
  ,
  "overlapsRange" ~: do
    (r, candidate, expected) <-
      [
        ((Closed 2, Open 5), (Closed 7, Open 10), False),
        ((Closed 2, Open 10), (Closed 3, Open 5), True),
        ((Closed 3, Open 5), (Closed 3, Open 5), True),
        ((Closed 2, Open 5), (Closed 3, Open 10), True),
        ((Closed 3, Open 5), (Closed 2, Open 10), True),
        ((Closed 3, Open 5), (Closed 1, Open 3), False),
        ((Closed 3, Open 5), (Closed 5, Open 7), False)
      ]
    let actual = r `overlaps` candidate
    return $ expected ~=? actual
  ]
{-# LANGUAGE DeriveFunctor #-}
module Range where

import Data.Functor.Classes (showsUnaryWith, Show1(liftShowsPrec))

data Endpoint a = Open a | Closed a deriving (Eq, Show, Functor)

instance Show1 Endpoint where
  liftShowsPrec sp _ d   (Open x) = showsUnaryWith sp   "Open" d x
  liftShowsPrec sp _ d (Closed x) = showsUnaryWith sp "Closed" d x

contains :: (Foldable t, Ord a) => (Endpoint a, Endpoint a) -> t a -> Bool
contains (lowerBound, upperBound) =
  let isHighEnough = case lowerBound of
        Closed x -> (x <=)
        Open   x -> (x <)
      isLowEnough = case upperBound of
        Closed y -> (<= y)
        Open   y ->  (< y)
      isContained x = isHighEnough x && isLowEnough x
  in all isContained

allPoints :: (Enum a, Num a) => (Endpoint a, Endpoint a) -> [a]
allPoints = uncurry enumFromTo . endpoints

endpoints :: (Num a1, Num a2) => (Endpoint a2, Endpoint a1) -> (a2, a1)
endpoints (Closed x, Closed y) = (x  , y)
endpoints (Closed x,   Open y) = (x  , y-1)
endpoints (  Open x, Closed y) = (x+1, y)
endpoints (  Open x,   Open y) = (x+1, y-1)

overlaps :: (Ord a1, Ord a2) =>
            (Endpoint a1, Endpoint a2) -> (Endpoint a2, Endpoint a1) -> Bool
overlaps (l1, h1) (l2, h2) =
  let less (Closed x) (Closed y) = x <= y
      less (Closed x)   (Open y) = x <  y
      less   (Open x) (Closed y) = x <  y
      less   (Open x)   (Open y) = x <  y
  in l1 `less` h2 && l2 `less` h1
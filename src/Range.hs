module Range where

import Control.Applicative (Applicative(liftA2))

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains :: (Foldable t, Ord a) => (Endpoint a, Endpoint a) -> t a -> Bool
contains (lowerBound, upperBound) candidate =
  let isHighEnough =
        case lowerBound of
          Closed x -> (x <=)
          Open   x -> (x <)
      isLowEnough =
        case upperBound of
          Closed y -> (<= y)
          Open   y ->  (< y)
  in all (liftA2 (&&) isHighEnough isLowEnough) candidate
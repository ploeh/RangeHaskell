module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains :: (Foldable t, Ord a) => (Endpoint a, Endpoint a) -> t a -> Bool
contains endpoints candidate =
  null candidate || (
  let low = minimum candidate
      hi  = maximum candidate
  in case endpoints of
      (Closed x, Closed y) -> x <= low && hi <= y
      (Closed x,   Open y) -> x <= low && hi  < y
      (  Open x, Closed y) -> x  < low && hi <= y
      (  Open x,   Open y) -> x  < low && hi  < y)
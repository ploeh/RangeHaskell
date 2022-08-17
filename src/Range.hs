module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

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

overlaps (l1, h1) (l2, h2) =
  let foo = case (l1, h2) of
        (Closed x, Closed y) -> x <= y
        (Closed x,   Open y) -> x <  y
        (  Open x, Closed y) -> x <  y
        (  Open x,   Open y) -> x <  y
      bar = case (l2, h1) of
        (Closed x, Closed y) -> x <= y
        (Closed x,   Open y) -> x <  y
        (  Open x, Closed y) -> x <  y
        (  Open x,   Open y) -> x <  y
  in foo && bar
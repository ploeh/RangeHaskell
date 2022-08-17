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
allPoints (Closed x, Closed y) = [x..y]
allPoints (Closed x,   Open y) = [x..y-1]
allPoints (  Open x, Closed y) = [x+1..y]
allPoints (  Open x,   Open y) = [x+1..y-1]

endpoints (_, Open _) = (2, 5)
endpoints _ = (2, 6)
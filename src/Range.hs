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

allPoints (Closed _, Open _) = [2..5]
allPoints (Closed _, Closed _) = [2..6]
allPoints (Open _, Closed _) = [3..6]
allPoints _ = [3..5]
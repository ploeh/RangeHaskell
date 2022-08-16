module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains (_, Open y) candidate = last candidate < y
contains (Open x, Closed y) candidate@(z:_) = x < z && last candidate <= y
contains (Closed x, _) (z:_) = x <= z
contains _ _ = False
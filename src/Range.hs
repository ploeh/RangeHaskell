module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains (Open x, Open y) candidate@(z:_) = x < z && last candidate < y
contains (Open x, Closed y) candidate@(z:_) = x < z && last candidate <= y
contains (Closed x, Open y) candidate@(z:_) = x <= z && last candidate < y
contains (Closed x, Closed y) candidate@(z:_) = x <= z && last candidate <= y
contains _ [] = True
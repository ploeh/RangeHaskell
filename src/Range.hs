module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains (_, Open 10) _ = False
contains (Closed x, _) (z:_) = x <= z
contains _ _ = False
module Range where

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains _ _ = True
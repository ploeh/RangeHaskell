module Range where

import Data.List

data Endpoint a = Open a | Closed a deriving (Eq, Show)

contains _ candidate = [2] `isPrefixOf` candidate
module Preterm where

data Preterm = PCompound String [Preterm]
             | PVariable String
             | PPlaceholder
               deriving (Show, Eq)

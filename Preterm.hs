module Preterm (
  Preterm(PCompound, PVariable, PPlaceholder),
  showPreterm
) where
import Data.List (intercalate)

data Preterm = PCompound String [Preterm]
             | PVariable String
             | PPlaceholder
               deriving (Show, Eq)

showPreterm :: Preterm -> String
showPreterm (PCompound sym []) = sym
showPreterm (PCompound sym args) =
  sym ++ "(" ++ intercalate ", " (map showPreterm args) ++ ")"
showPreterm (PVariable varname) = varname
showPreterm PPlaceholder = "_"

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
showPreterm (PCompound "[]" []) = "[]"
showPreterm (PCompound "." [t0, t1]) = "[" ++ showPretermList t0 t1 ++ "]"
showPreterm (PCompound sym []) = sym
showPreterm (PCompound sym args) =
  sym ++ "(" ++ intercalate ", " (map showPreterm args) ++ ")"
showPreterm (PVariable varname) = varname
showPreterm PPlaceholder = "_"

showPretermList :: Preterm -> Preterm -> String
showPretermList t0 (PCompound "[]" []) = showPreterm t0
showPretermList t0 (PCompound "." [t1, t2]) =
  showPreterm t0 ++ ", " ++ showPretermList t1 t2
showPretermList t0 t1 =
  showPreterm t0 ++ " | " ++ showPreterm t1

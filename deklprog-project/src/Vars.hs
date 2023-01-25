module Vars (Vars (allVars), freshVars) where

import Base.Type
import Text.Read (Lexeme(String))
import Data.List

class Vars a where
  -- returns all vars
  allVars :: a -> [VarName]

-- get all vars of a term
instance Vars Term where
  allVars (Var name) = [name]
  allVars (Comb _ ts) = nub (concatMap allVars ts)

-- get all vars of a rule
instance Vars Rule where
  allVars (Rule t ts) = nub (allVars t ++ concatMap allVars ts)

-- get all vars of a prog
instance Vars Prog where
  allVars (Prog rules) = nub (concatMap allVars rules)

-- get all vars of goal
instance Vars Goal where
  allVars (Goal terms) = nub (concatMap allVars terms)

-- list of all possible VarNames
freshVars :: [VarName]
freshVars = possibleVarNames (['0'..'9'] ++ ['A'..'Z']) "" [] [] where
  possibleVarNames :: [Char] -> String -> [String] -> [String] -> [VarName]
  possibleVarNames [] _ [] allStrings = possibleVarNames [] "" allStrings allStrings
  possibleVarNames [] _ (u:us) allStrings = possibleVarNames (['0'..'9'] ++ ['A'..'Z']) u us allStrings
  possibleVarNames (c:cs) string usedStrings allStrings=
    let newString = c: string
    in if c `elem` ['0'..'9'] 
      then possibleVarNames cs string usedStrings (allStrings ++ [newString])
      else VarName newString : possibleVarNames cs string usedStrings (allStrings ++ [newString])
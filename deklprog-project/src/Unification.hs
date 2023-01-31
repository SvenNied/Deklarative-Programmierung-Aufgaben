{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( testUnification
  , ds , unify
  ) where


import Data.Maybe
import Base.Type
import Subst
import Vars

import Test.QuickCheck

-- Properties
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x)     (Var y)     | x == y    = Nothing
                           | otherwise = Just (Var x, Var y)
ds (Var x)     y           = Just (Var x, y)
ds x           (Var y)     = Just (Var y, x)

ds (Comb f ft) (Comb g gt)            = if(f /= g || (length ft /= length gt)) 
                                          then Just ((Comb f ft), (Comb g gt))
                                                 else firstDsagInList ft gt  
  where                
    firstDsagInList :: [Term] -> [Term] -> Maybe(Term, Term)
    firstDsagInList [] [] = Nothing
    firstDsagInList [] _ = undefined
    firstDsagInList _ [] = undefined
    firstDsagInList (term1:terms1) (term2:terms2) = if (ds term1 term2 == Nothing) 
                                                      then firstDsagInList terms1 terms2
                                                      else ds term1 term2                       

-- determines the mgu
unify :: Term -> Term -> Maybe Subst
unify term1 term2 = (mgu empty (ds term1 term2))
  where
    mgu :: Subst -> Maybe (Term, Term) -> Maybe Subst
    mgu subst Nothing = Just subst
    mgu subst (Just ((Var varname), t)) = 
      let newSubst = (compose (single varname t) subst)
      in if varname `notElem` (allVars t)
          then mgu newSubst (ds (apply newSubst term1) (apply newSubst term2))
          else Nothing
    mgu _ _ = Nothing
    



-- Uncomment this to test the properties when all required functions are implemented

-- Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t

-- The disagreement set of a term with itself is empty
prop_1 :: Term -> Bool
prop_1 t = isNothing (ds t t)

-- The disagreement set of two different terms is not empty
prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

-- If a variable v occurs in a term t (other than the variable itself),
-- then the unification of v and t should fail due to the occur check
prop_3 :: VarName -> Term -> Property
prop_3 v t = occurs v t && t /= Var v ==> isNothing (unify (Var v) t)

-- If two terms t1 and t2 are unifiable, then the disagreement set of the mgu
-- applied to t1 and t2 is empty
prop_4 :: Term -> Term -> Property
prop_4 t1 t2 =
  let mMgu = unify t1 t2
  in isJust mMgu ==> let mgu = fromJust mMgu
                     in isNothing (ds (apply mgu t1) (apply mgu t2))


-- Run all tests
return []
testUnification :: IO Bool
testUnification = $quickCheckAll

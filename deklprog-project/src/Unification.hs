{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( testUnification
  , ds , unifiy
  ) where


import Data.Maybe

import Test.QuickCheck

-- Properties
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var x)     (Var y)     | x == "_"  = Nothing
                           | y == "_"  = Nothing
                           | x == y    = Nothing
                           | otherwise = Just (Var x, Var y)
ds (Var x)     y           | x == "_"  = Nothing
                           | otherwise = Just (Var x, y)
ds x           (Var y)     | y == "_"  = Nothing          
                           | otherwise = Just (Var y, x)

ds (Comb f ft) (Comb g gt)            = if(f /= g || (length ft /= length gt)) 
                                          then Just ((Comb f ft), (Comb g gt))
                                                 else firstDsagInList ft gt  
  where                
    firstDsagInList :: [Term] -> [Term] -> Maybe(Term, Term)
    firstDsagInList [] [] = Nothing
    firstDsagInList (term1:terms1) (term2:terms2) = if (ds a b == Nothing) 
                                                      then firstDsagInList as bs
                                                      else ds a b                       

-- determines the mgu
unify :: Term -> Term -> Maybe Subst
unifiy term1 term2 = (mgu term1 term2) empty
        where
            mgu subst term1 term2 = if ds (apply subst term1) (apply subst term2) == Just (Var varname, someTerm) || ds (apply subst term1) (apply subst term2) == Just (someTerm, Var varname)
                                      then if (varname 'elem' allVars someTerm) then Nothing else mgu (compose (single varname someTerm) subst) term1 term2  
                                        else if (ds (apply subst term1) (apply subst term2) == Just _ ) then Nothing
                                          else if (ds (apply subst term1) (apply subst term2) == Nothing) then Just subst

{- 

Uncomment this to test the properties when all required functions are implemented

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
-}

-- Run all tests
testUnification :: IO Bool
testUnification = undefined

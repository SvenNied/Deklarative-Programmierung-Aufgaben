{-# LANGUAGE TemplateHaskell #-}

module Subst
  ( Subst(Subst), -- don't export the constructor of the data type!
    domain,
    empty,
    single,
    compose,
    apply,
    restrictTo,
    testSubst,
    isEmpty
  )
where

import Base.Type
import Data.List (intercalate, nub, sort)
import Test.QuickCheck
import Test.QuickCheck.All
import Vars
import Pretty

-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving (Show)

-- Generator for substitutions
instance Arbitrary Subst where
  -- We use the `suchThat` combinator to filter out substitutions that are not valid,
  -- i.e. whose domain contains the same variable more than once.
  arbitrary = Subst <$> (arbitrary `suchThat` ((\vts -> length vts == length (nub vts)) . map fst))

-- returns the domain of a substitution
domain:: Subst -> [VarName]
domain (Subst []) = []
domain (Subst ((varName, term): xs)) = if ((Var varName) == term)
  then domain (Subst xs)
  else varName : domain (Subst xs)

-- restricts a substitution to the given varNames
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst mappings) varNames = Subst (filter (\(varName,_) -> varName `elem` varNames) mappings)

-- returns an empty substitution
empty :: Subst
empty = Subst []

-- returns a substitution of one varName for the given term
single :: VarName -> Term -> Subst
single varName term = Subst [(varName,term)|(Var varName) /= term]

isEmpty :: Subst -> Bool
isEmpty subst = domain subst == []

-- applies a substituition
apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst (replacement: rs)) term = apply (Subst rs) (applyToTerm replacement term)
  where
    applyToTerm (rName,rTerm) (Var tName) = if tName == rName then rTerm else Var tName
    applyToTerm r (Comb cName terms) = Comb cName (map (\t -> applyToTerm r t) terms)

-- composes two substitutions
compose :: Subst -> Subst -> Subst
compose (Subst replacementsY) (Subst replacementsX) = 
  Subst ([(name,(apply (Subst replacementsY) term))|(name,term)<-replacementsX,(apply (Subst replacementsY) term) /= Var name] 
  ++ [(name,term)|(name,term)<-replacementsY,name `notElem` (domain (Subst replacementsX))])

-- pretty printing of substitutions
instance Pretty Subst where
  pretty (Subst replacements) = '{' : intercalate ", " (map prettyReplacement replacements) ++ "}"
    where
      prettyReplacement ((VarName varName), term) = if (VarName varName) `elem` domain (Subst replacements) then varName ++ " -> " ++ pretty term else ""

-- listing vars of substitutions
instance Vars Subst where
  allVars (Subst []) = []
  allVars (Subst ((varName, term): rs)) = nub (varName : allVars (term) ++ allVars( Subst rs))
-- Properties

-- Uncomment this to test the properties when all required functions are implemented

-- Applying the empty substitution to a term should not change the term
prop_1 :: Term -> Bool
prop_1 t = apply empty t == t

-- Applying a singleton substitution {X -> t} to X should return t
prop_2 :: VarName -> Term -> Bool
prop_2 x t = apply (single x t) (Var x) == t

-- Applying a composed substitution is equal to applying the two substitutions individually
prop_3 :: Term -> Subst -> Subst -> Bool
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

-- The domain of the empty substitution is empty
prop_4 :: Bool
prop_4 = null (domain empty)

-- The domain of a singleton substitution {X -> X} is empty
prop_5 :: VarName -> Bool
prop_5 x = null (domain (single x (Var x)))

-- The domain of a singleton substitution {X -> t} is [X]
prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= Var x ==> domain (single x t) == [x]

-- The domain of a composed substitution is the union of the domains of the two substitutions
prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = all (`elem` (domain s1 ++ domain s2)) (domain (compose s1 s2))

-- The domain of a composed substitution does not contain variables that are mapped to themselves
prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 =
  x1
    /= x2
    ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2)))
    == [x2]

-- The empty substitution does not contain any variables
prop_9 :: Bool
prop_9 = null (allVars empty)

-- The singleton substitution should not map a variable to itself
prop_10 :: VarName -> Bool
prop_10 x = null (allVars (single x (Var x)))

-- The variables occuring in a subsitution should be taken from both components of the individual substitutions
prop_11 :: VarName -> Term -> Property
prop_11 x t =
  t
    /= Var x
    ==> sort (nub (allVars (single x t)))
    == sort (nub (x : allVars t))

-- The variables occuring in a composed substitution are a subset of the variables occuring in the two substitutions
prop_12 :: Subst -> Subst -> Bool
prop_12 s1 s2 =
  all (`elem` (allVars s1 ++ allVars s2)) (allVars (compose s1 s2))

-- The composed subsitution should contain the left substitution unless its variables are mapped by the right substitution
prop_13 :: VarName -> VarName -> Property
prop_13 x1 x2 =
  x1
    /= x2
    ==> sort (allVars (compose (single x2 (Var x1)) (single x1 (Var x2))))
    == sort [x1, x2]

-- The domain of a substitution is a subset of all its variables
prop_14 :: Subst -> Bool
prop_14 s = all (`elem` allVars s) (domain s)

-- Restricting the empty substitution to an arbitrary set of variables should return the empty substitution
prop_15 :: [VarName] -> Bool
prop_15 xs = null (domain (restrictTo empty xs))

-- The domain of a restricted substitution is a subset of the given set of variables
prop_16 :: [VarName] -> Subst -> Bool
prop_16 xs s = all (`elem` xs) (domain (restrictTo s xs))

-- Run all tests
return []
testSubst :: IO Bool
testSubst = $quickCheckAll
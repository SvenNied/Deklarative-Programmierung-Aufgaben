module SLD
  ( SLDTree (..),
    sld,
    -- Strategy,
    -- dfs,
    -- bfs,
    -- solveWith,
  )
where

import Base.Type
import Subst
import Unification
import Data.Maybe (catMaybes)

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal = SLDTree goal []
sld _ (Goal []) = SLDTree (Goal []) []
sld prog@(Prog rules) goal@(Goal (firstTerm:_))  = SLDTree goal (catMaybes (map (applyRule firstTerm) rules))
  where 
    applyRule:: Term -> Rule -> Maybe (Subst,SLDTree)
    applyRule term (Rule ruleTerm replacements) = applySubst (unify term ruleTerm) term
    applySubst Nothing _ = Nothing
    applySubst (Just subst) term = Just (subst, sld prog (Goal [(apply subst term)]))

dfs :: Strategy
dfs = undefined

bfs :: Strategy
bfs = undefined

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal strat = strat (sld prog goal)
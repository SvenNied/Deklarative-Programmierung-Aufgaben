module SLD
  ( SLDTree (..),
    sld,
    Strategy,
    dfs,
    bfs,
    solveWith,
  )
where

import Base.Type
import Subst
import Unification
import Data.List
import Rename
import Data.Maybe (catMaybes)

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

type Strategy = SLDTree -> [Subst]

sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal = SLDTree goal []
sld _ (Goal []) = SLDTree (Goal []) []
sld prog@(Prog rules) goal@(Goal (firstTerm:_))  = SLDTree goal (catMaybes (map (\rule -> applyRule firstTerm (rename [] rule)) rules))
  where 
    applyRule:: Term -> Rule -> Maybe (Subst,SLDTree)
    applyRule term (Rule ruleTerm replacements) = applySubst (unify term ruleTerm) term replacements
    applySubst Nothing _ _= Nothing
    applySubst (Just subst) term replacements = Just (subst, sld prog (Goal replacements))

dfs :: Strategy
dfs (SLDTree _ []) = []
dfs (SLDTree _ nodes) = concatMap handleNode nodes
  where
    handleNode:: (Subst,SLDTree) -> [Subst]
    handleNode (subst, tree) = map (compose subst) (dfs tree)

bfs :: Strategy
bfs (SLDTree _ []) = []
bfs (SLDTree _ nodes) = map (\(subst,_)-> subst) (filter nodeWithEmptyTree nodes) ++ concatMap bfs (map (\(_,tree) -> tree) (filter (not . nodeWithEmptyTree) nodes))
  where
    nodeWithEmptyTree (_,(SLDTree _ [])) = True
    nodeWithEmptyTree (_,(SLDTree _ _)) = False

solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith prog goal strat = strat (sld prog goal)
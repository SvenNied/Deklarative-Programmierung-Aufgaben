import Data.Time.Format.ISO8601 (yearFormat)
data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

flatTree   :: Tree (Tree a) -> Tree a
flatTree = foldTree fLeaf Branch
    where 
        fLeaf (Branch x y) = Branch x y
        fLeaf (Leaf x) = Leaf x

mapTree    :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\x -> Leaf (f x)) Branch  

foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree fLeaf fBranch input = case input of
    Leaf x -> fLeaf x
    Branch x y -> fBranch (foldTree fLeaf fBranch x) (foldTree fLeaf fBranch y)

extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f x = flatTree (mapTree f x)
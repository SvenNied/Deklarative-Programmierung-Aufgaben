data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

-- falltens a tree of trees into a single tree
flatTree   :: Tree (Tree a) -> Tree a
flatTree = foldTree fLeaf Branch
    where 
        fLeaf (Branch x y) = Branch x y
        fLeaf (Leaf x) = Leaf x

-- executes the function on every leaf and returns the resuling tree
mapTree    :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\x -> Leaf (f x)) Branch  

-- executes one function on each leaf and the other on each node, retunrs the resulting tree
foldTree   :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree fLeaf fBranch input = case input of
    Leaf x -> fLeaf x
    Branch x y -> fBranch (foldTree fLeaf fBranch x) (foldTree fLeaf fBranch y)

-- extends eery node into a tree using the specified function, returns the flattened tree
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f x = flatTree (mapTree f x)
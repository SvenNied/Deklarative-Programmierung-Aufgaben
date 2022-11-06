data Tree a = Tree (Tree a) (Tree a)| Leaf a deriving Show

combineTree :: Tree a -> Tree a -> Tree a
combineTree left right = Tree left right

singletonTree :: a -> Tree a
singletonTree n = Leaf n

sumTree :: Tree Int -> Int
sumTree (Leaf n) = n
sumTree (Tree left right) = sumTree left + sumTree right

mirrorTree :: Tree a -> Tree a
mirrorTree (Leaf a) = Leaf a
mirrorTree (Tree left right) = Tree (mirrorTree right) (mirrorTree left)

-- Returns a list of all elements in the binary tree
-- Complexity of n because the function is excuted once for each node and we have n leafs and n -1 tree nodes
toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Tree left right) = (toList left) ++ (toList right)
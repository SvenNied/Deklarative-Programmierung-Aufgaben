reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x: xs) = reverse xs ++ [x]

-- Returns the index of the given int in the list; Nothing if int not n list
indexOf :: Int -> [Int] -> Maybe Int
indexOf n list = go n list 0
    where
        go _ [] _ = Nothing
        go element (x: xs) index = if x == element then Just index else go element xs (index + 1)

-- returns all possible start pieces of given list
inits :: [a] -> [[a]]
inits [] = [[]]
inits list = go list []
    where
        go :: [a] -> [a] -> [[a]]
        go [] result = [result]
        go (x: xs) result = result : go xs (result ++ [x])

-- returns all possible end pece of list
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- retuns a list of list withn n inserted at every possible index
insert :: a -> [a] -> [[a]]
insert _ [] = []
insert n list = go [] n list
    where 
        go :: [a] -> a -> [a] -> [[a]]
        go prefix element [] = [prefix ++ [element]]
        go prefix element (x: xs) = (prefix ++ (element:  x : xs)) : go (prefix ++ [x]) element xs

-- returns all permutations of the list
perms :: [a] -> [[a]]
perms [] = [[]]
perms list = go [] list
    where
        go :: [a] -> [a] -> [[a]]
        go _ [] = []
        go usedElements (x : xs) = (prepend x (perms (usedElements ++ xs))) ++ (go (usedElements ++ [x]) xs) 
        
        prepend :: a -> [[a]] -> [[a]]
        prepend _ [] = []
        prepend prefix (l : ls) = [(prefix : l)] ++ prepend prefix ls
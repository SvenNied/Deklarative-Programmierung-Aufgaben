module Exercises.Week3 where
import Test.QuickCheck
--Checks for reverse1
--We have to check if the length of the two list 
--is still the the same after reversing
prop_reverse_listHasTheSameLength :: [Int] -> Bool
prop_reverse_listHasTheSameLength a = length a == length (reverse1 a )

--Test for indexOf
--We have to check that there is indeed the element we searched for
-- at the found Index
prop_elementIsAtIndex :: Int -> [Int] -> Bool 
prop_elementIsAtIndex n list = case indexOf n list of
  Nothing -> not (n `elem` list)
  Just result -> list!!result == n

--Test for inits
--We have to check that the number of elements in the list is increasing by one
prop_increasingLists :: [Int] -> Bool
prop_increasingLists list = isIncreasing 0 (inits list)
  where 
    isIncreasing _ [] = True
    isIncreasing l (x:xs) = length x == l && isIncreasing (l+1) xs

-- Test for tails
--We have to check that the number of elements in the list is decreasing by one
prop_decreasingLists :: [Int] -> Bool
prop_decreasingLists list = isDecreasing (length list) (tails list)
  where 
    isDecreasing _ [] = True
    isDecreasing l (x: xs) = length x == l && isDecreasing (l - 1) xs

--Test for insert
--We have to check if every Element in the list has the same length
prop_insertedListhaveSameLength :: Int -> [Int] -> Bool
prop_insertedListhaveSameLength element list = allElementsHaveThisLength (length list + 1) (insert element list)
  where 
    allElementsHaveThisLength:: Int -> [[Int]] -> Bool
    allElementsHaveThisLength _ []             = True  
    allElementsHaveThisLength n (x: xs) = n == length x && allElementsHaveThisLength n xs



--Test for perms
--There wether the correct number of permutatiosn is generated
prop_checkPermutationCount:: [Int] -> Property
prop_checkPermutationCount list = length list < 5 ==> length (perms list) == fac (length list)
  where
    fac n = if n == 0 then 1 else n * fac (n - 1)

-- Returns the list in reverse order
-- This function has a complexity of n^2 beacuse it is called n times and adds an element at the end of a linked list wich in iself has complexity n
-- Calling this function with 100000 list elments took 11.75 seconds
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x: xs) = reverse xs ++ [x]

-- Returns the list in reverse order
-- This function has a complexity of n, because were are inserting at the beginning of the linked list wich has a compexity of 1
-- Calling this function with 100000 took 6 seconds, the difference to reverse1 is relatively small, because printing the whole array introducies significatn overhead
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 list = go list []
    where 
        go [] reversed = reversed
        go (x: xs) reversed = go xs (x : reversed)

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
insert n list = go [] n list
    where 
        go :: [a] -> a -> [a] -> [[a]]
        go prefix element [] = [prefix ++ [element]]
        go prefix element (x: xs) = (prefix ++ (element:  x : xs)) : go (prefix ++ [x]) element xs

-- returns all permutations of the list
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = permInsert [] (perms xs)
  where
    permInsert acc []       = acc
    permInsert acc (ys:yss) = permInsert (acc ++ insert x ys) yss
type IntSet = Int -> Bool

-- returns an empty set
empty :: IntSet
empty _ = False

-- inserts an integer into a set
insert :: Int -> IntSet -> IntSet
insert n set = \x -> set x || n == x

-- removes an integer from a set
remove :: Int -> IntSet -> IntSet
remove n set = \x -> x /= n && set n

-- checks wether an integer is part of a set
isElem :: Int -> IntSet -> Bool
isElem n set = set n

-- converts a list to a set
listToSet:: [Int] -> IntSet
listToSet n = \x -> x `elem` n

-- combines two sets
union :: IntSet -> IntSet -> IntSet
union a b = \x -> a x || b x

-- intersects two sets
intersection :: IntSet -> IntSet -> IntSet
intersection a b = \x -> a x && b x

-- reurns the difference between tow sets
difference :: IntSet -> IntSet -> IntSet 
difference a b = \x -> a x && not (b x)

-- returns the complement of a set
complement :: IntSet -> IntSet
complement set = \x -> not(set x)

-- converts a set to a list by brute-forcing every possible integer 
-- this is theoretically possible, however it may not be very practical due to the high runtime
setToList :: IntSet -> [Int]
setToList set = ints minBound
    where 
        ints:: Int -> [Int]
        ints n = 
            let rest = if n < maxBound then (ints (n + 1)) else [] 
            in if set n then n: rest else rest
data Rose a = Rose a [Rose a]
  deriving Show

-- Implements the Ord class for rose-trees
instance (Ord a) => Ord (Rose a) where 
    (<=) (Rose leftElement leftList) (Rose rightElement rightList) = 
        if leftElement < rightElement then True
        else if leftElement > rightElement then False
        else listCompare leftList rightList
        where 
            listCompare [] [] = True
            listCompare [] _ = True
            listCompare _ [] = False
            listCompare (l:ls) (r:rs)
                | l == r = listCompare ls rs 
                | l <= r = True
                | otherwise = False

-- implements the eq class for rose trees
instance (Eq a) => Eq (Rose a) where
    (==) (Rose leftElement leftList) (Rose rightElement rightList) =
        leftElement == rightElement && listCompare leftList rightList
        where
            listCompare [] [] = True
            listCompare (_:_) [] = False
            listCompare [] (_:_) = False
            listCompare (l:ls) (r:rs) = r == l && listCompare ls rs

-- defines a class for pretty-printing
class Pretty a where
    pretty::  a -> String 

--impements the Pretty class for rose trees
instance (Pretty a) => Pretty (Rose a) where
    pretty (Rose start completeList) = pretty start ++ printList 1 completeList
        where
            printRose n (Rose element list)  = "\n" ++ prefix n ++ pretty element ++ printList (n + 1) list
            prefix n 
                | n == 0 = ""
                | n == 1 = "+-- "
                | otherwise = "|   " ++ prefix (n - 1)
            printList _ [] = ""
            printList n (x:xs) = printRose n x ++ printList n xs

-- implements the Pretty class for ints
instance Pretty Int where
    pretty n = show n

-- implements the Pretty class for doubles
instance Pretty Double where
    pretty = show

-- implements the Pretty class for floats
instance Pretty Float where 
    pretty = show
import Prelude hiding (map, lookup, filter, replicate)
import Data.Maybe
list1:: [[Int]]
list1 = [ [j] | i <- [1 .. 5], j <- [2*i+1]]

list2 :: [(Int, Bool)]
list2 = [ (if i == 1 then 5 else if i == 2 then 20 else 25,j) | i <- [1 .. 5], j <- if i < 4 then [i `mod` 2 == 0] else []]

list3:: [Maybe Int]
list3 = [ (Just j) | i <- [1 .. 5], j <- if i `mod` 2 == 1 then [i * i] else []]

list4 :: [(Int, Int)]
list4 = [ (i,6-j+i) | i <- [1 .. 5], j <- if i < 5 then [i+1..5] else []]

map :: (a->b) -> [a] -> [b]
map f l = [f i|i<-l]

lookup :: Eq a => a -> [(a, b)] -> Maybe b 
lookup key dict = listToMaybe [snd i|i<-dict,j<- if fst(i) == key then [1] else []]

replicate :: Int -> a -> [a] 
replicate n element = [j|i<-[1..n],j<-[element]]

filter :: (a -> Bool) -> [a] -> [a] 
filter condition list = [i|i<-list,j<-if condition i then [1] else []]
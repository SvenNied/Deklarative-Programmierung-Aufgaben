module Exercises.Week3 where
import Test.QuickCheck
--Checks for reverse1
--We have to check if the length of the two list 
--is still the the same after reversing
prop_listHasTheSameLength :: a -> Bool
prop_listHasTheSameLength = length (a) == length (reverse1(a))

--Test for indexOf
--We have to check that there is indeed the element we searched for
-- at the found Index
prop_RightElementWasFound :: Int -> [Int] -> Bool 
prop_RightElementWasFound n list = goToIndex indexOf n list == n
    where
      goToIndex _ [] = Nothing
      goToIndex 0 (x:xs) = x 
      goToIndex i (x:xs) = goToIndex (i-1) xs    

--Test for inits
--We have to check that the number of elements in the list is increasing by 
--one
prop_increasingLists : [a] -> Bool
prop_increasingLists [a] = isIncreasing inits [a]
  where 
    isIncreasing [] =True
    isIncreasing x:[] = True
    isIncreasing (x:y:list) = if (length x + 1) == length y
                                then True && isIncreasing y:list
                                  else False
--Test for tails
--We have to check that the number of elements in the list is decreasing by 
--one
prop_decreasingLists : [a] -> Bool
prop_decreasingLists [a] = isDecreasing tails [a]
  where 
    isDecreasing [] = True
    isDecreasing (x:y:list) = if (length x - 1) == length y
                                then True && isDecreasing y:list
                                  else False
--Test for insert
--We have to check if every Element in the list has the same length
prop_sameLength :: a [a] -> Bool
prop_sameLength = allElementsHaveThisLength length [a] + 1 insert a [a]
  where allElementsHaveThisLength _ []             = True  
        allElementsHaveThisLength n [element list] = n == length element && allElementsHaveThisLength n [list]



--Test for perms
--There should be length -1 list faculty many elements that start with every element.
fac :: Int → Int
fac n = if n == 0 then 1 else n * fac (n - 1)

prop_enoughHeadsForEveryElement :: [a] -> Bool
prop_enoughHeadsForEveryElement (element : list) = NumberOfHeadsForThisElement element perms (element : list) == fac(length(element:list)-1)
  where NumberOfHeadsForThisElement element  [] = 0  
        NumberOfHeadsForThisElement element  ((x:xs):xss) = if element == x
                                                              then 1 + NumberOfHeadsForThisElement element xss
                                                                else  NumberOfHeadsForThisElement element xss 

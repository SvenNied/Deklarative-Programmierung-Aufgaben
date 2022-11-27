--Here we decide whether a number is Avalanche or not.
isavalanche :: Integer -> Bool
isavalanche 1 = True
isavalanche n = 
                if n `mod` 3 == 0 then isavalanche (n `div` 3)
                else
                     if n `mod` 5 == 0 then isavalanche (n `div` 5)
                else 
                    if n `mod` 7 == 0 then isavalanche (n `div` 7)
                else False

--Generating the list of Avalanche numbers
avalanchegen :: Integer -> [Integer]
avalanchegen n = if (isavalanche n) 
                   then (n : avalanchegen (n+1))
                   else avalanchegen (n+1)
--We generate the whole list by starting at the number 1
avalanche :: [Integer]
avalanche = avalanchegen 1


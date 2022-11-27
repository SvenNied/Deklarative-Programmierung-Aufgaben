-- Generate an infinite list of all possible combinations of the elements in the given list
allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations list = [] : go list [[]] []
    where 
        go [] (_:ps) result = go list ps result
        go (x:xs) (prefix: prefixes) result = 
            let currentStep = prefix ++ [x]
            in currentStep: go xs (prefix: prefixes) (result ++ [currentStep])
        go _ [] result = go list result []
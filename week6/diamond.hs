-- prints a diamond pixel art
diamond :: Int -> IO ()
diamond 0 = putStrLn("")
diamond levels = putStrLn (getDiamondString (2*levels -1) (levels * 2 -1))
    where
        spaces:: Int -> String
        spaces n = take n (repeat ' ')
        getDiamondString:: Int -> Int -> String
        getDiamondString 0 _ = ""
        getDiamondString index width =
            let currentLevel = if index < levels then index -1 else width - index
            in 
                if currentLevel == 0 then  "\n" ++ spaces(width `div` 2) ++ "*" ++ spaces( width `div` 2) ++  getDiamondString (index - 1) width
                else 
                    let innerWidth = 2 * currentLevel - 1;
                        outerSpaces = spaces ((width - (innerWidth + 2)) `div` 2)
                    in "\n" ++ outerSpaces ++ "*" ++ spaces innerWidth ++ "*" ++ outerSpaces ++  getDiamondString (index - 1) width

-- prints a christmas tree pixel art
tree :: Int -> IO () 
tree size = 
    if size < 3 then putStrLn " " else putStrLn (treeString  (2 * size - 2))
    where
        treeString 0 = ""
        treeString n =
            let width = 2 * size - 1
                starCount = (size * 2 - n -2) * 2 + 1
                stars = take (starCount) (repeat '*')
                spaces = take ((width - starCount) `div` 2) (repeat ' ')
            in
                if n <= size - 2
                then  "\n" ++ take (width `div` 2) (repeat ' ') ++ "*" ++ take (width `div` 2) (repeat ' ') ++ treeString (n - 1)
                else "\n" ++ spaces ++ stars ++ spaces ++ treeString (n - 1)


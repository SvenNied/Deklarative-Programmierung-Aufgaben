import Data.Char (toLower)

-- Starts a game of hangman with the specified secret word
hangman:: String -> IO ()
hangman word = gameloop [] 0
    where
        redacted:: String -> [Char] -> String
        redacted [] _ = []
        redacted (c:cs) guesses = if toLower c `elem` guesses then c : (redacted cs guesses) else '*' : (redacted cs guesses)
        gameloop guesses tries =
            let redactedWord = redacted word guesses
            in 
                if redactedWord == word then 
                    putStrLn ("Solved in " ++ show tries ++ " tries.")
                else do
                    putStrLn ("Secret: " ++ redactedWord)
                    putStr "Enter a character: "
                    ch <- getChar
                    putStrLn ""
                    gameloop (toLower ch: guesses) (tries + 1)
        
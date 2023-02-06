module Main (main, printAnswers, parseCommand) where

import SLD
import Base.Type
import Base.Parser
import Subst
import Base.IOHelper
import Pretty

-- Main function
main :: IO ()
main = do 
    title
    programLoop dfs (Prog [])
  where 
    programLoop:: Strategy -> Prog -> IO ()
    programLoop strategy prog = do
      putStr "?- "
      flushBuffer
      cmdString <- getLine
      case (parseCommand cmdString) of
        (Left errorMessage) -> do
          print errorMessage
          programLoop strategy prog
        (Right command) -> case command of
          Help -> do 
            putStr help
            programLoop strategy prog
          (Load path) -> do
            possibleProg <- parseFile path
            case (possibleProg) of
              (Left errorString) -> do
                putStr (errorString ++ "\n")
                programLoop strategy prog
              (Right newProg) -> do
                putStr ("Loaded program: " ++ path ++ "\n")
                programLoop strategy newProg
          Quit -> putStr "Goodbye\n"
          (SetStrategy newStrategy) -> do
            putStr "Strategy changed\n"
            programLoop newStrategy prog
          (SolveGoal goal) -> printAnswers (solveWith prog goal strategy) (programLoop strategy prog)

-- Title screen
title :: IO ()
title = do
  putStrLn " ___ _       ___ _ "
  putStrLn "/ __(_)_ __ | _ \\ |    Simple Prolog"
  putStrLn "\\__ \\ | '  \\|  _/ |__   Version 2023"
  putStrLn "|___/_|_|_|_|_| |____|"
  putStrLn ""
  putStrLn "We apologize for the lame joke"
  putStrLn "Type \":h\" for help."

-- Possible commands
data Command = Help | Load FilePath | Quit | SetStrategy Strategy | SolveGoal Goal

type ErrorMessage = String

parseCommand :: String -> Either ErrorMessage Command
parseCommand (':':str) = case str of
  "h"             -> Right Help
  "q"             -> Right Quit
  ('l':' ':fn)    -> Right (Load fn)
  ('s':' ':strat) -> case strat of
    "dfs" -> Right (SetStrategy dfs)
    "bfs" -> Right (SetStrategy bfs)
    _     -> Left "Unknown strategy"
  _ -> Left "Unknown command"
parseCommand str = case parse str of
  Left err -> Left err
  Right g -> Right (SolveGoal g)

printAnswers :: [Subst] -> IO () -> IO ()
printAnswers []         io = do
  putStrLn "No more solutions."
  io
printAnswers (ans:anss) io = do
  putStr (pretty ans)
  c <- getKeypress -- getChar
  putStrLn ""
  if c `elem` "; n"
    then printAnswers anss io
    else io

help :: String
help = "Commands available from the prompt:\n\
  \<goal>      Solves/proves the specified goal.\n\
  \:h          Shows this help message.\n\
  \:l <file>   Loads the specified file.\n\
  \:q          Exits the interactive environment.\n\
  \:s <strat>  Sets the specified search strategy\n\
  \            where <strat> is one of 'dfs' or 'bfs'.\n"
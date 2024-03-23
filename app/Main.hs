module Main where

import Data.Foldable (traverse_)
import System.Environment (getArgs)

import MyLib (
  Line,
  SearchTerm,
  caseSensitiveSearch,
  caseInsensitiveSearch,
  caseSensitiveSearchWithLineNumbers,
  caseInsensitiveSearchWithLineNumbers)

main :: IO ()
main = do
  args <- getArgs
  
  case length args of
    0 -> printMsgForMissingArgs

    1 | "--help" `elem` args || "-h" `elem` args -> printHelpText
      | otherwise -> do
        putStrLn "Invalid argument provided!"
        putStrLn "\n"
        printUsageInstructions

    4 -> do
      let (_ : fileName : _ : searchTerm : _) = args
      contents <- readFile fileName
      let inputLines = lines contents
          res = caseSensitiveSearch searchTerm inputLines
      if null res 
        then printMsgForNoMatchesFound searchTerm
        else printLines res

    5 -> do
      let (flag : _ : fileName : _ : searchTerm : _) = args

      if flag == "-i" || flag == "--case-insensitive"
        then do
          contents <- readFile fileName
          let inputLines = lines contents 
              res = caseInsensitiveSearch searchTerm inputLines
          if null res 
            then printMsgForNoMatchesFound searchTerm
            else printLines res

      else if flag == "-l" || flag == "--line-numbers"
        then do
          contents <- readFile fileName
          let inputLines = lines contents 
              res = caseSensitiveSearchWithLineNumbers searchTerm inputLines
          if null res 
            then printMsgForNoMatchesFound searchTerm
            else printLines res

      else do
          putStrLn "Invalid flag entered!"
          putStrLn "\n"
          printAvailableOptions
    
    6 -> do
      let (fileName : _ : searchTerm : _) = drop 3 args
      contents <- readFile fileName
      let inputLines = lines contents 
          res = caseInsensitiveSearchWithLineNumbers searchTerm inputLines
      if null res 
        then printMsgForNoMatchesFound searchTerm
        else printLines res

    _ -> do 
      putStrLn "Something seemed wrong with the provided arguments!"
      putStrLn "\n"
      printUsageInstructions

printLines :: [Line] -> IO ()
printLines = traverse_ putStrLn

printHelpText :: IO ()
printHelpText = do
  putStrLn "ezfind - A simpler grep!"
  putStrLn "\n"
  printUsageInstructions
  putStrLn "\n"
  printAvailableOptions

printUsageInstructions :: IO ()
printUsageInstructions = do
  putStrLn "Usage: ezfind [-i|--case-insensitive] [-l|--line-numbers] (-f|--file FILE) (-t|--term TERM)"
  putStrLn "\n"
  putStrLn "Search for a TERM in a text FILE"

printAvailableOptions :: IO ()
printAvailableOptions = do
  putStrLn "Available options:"
  putStrLn "  -i,--case-insensitive\tSet case-insensitive search (disabled by default)"
  putStrLn "  -l,--line-numbers\tPrint line numbers"
  putStrLn "  -f,--file FILE\tPath of the file to search for term in"
  putStrLn "  -t,--term TERM\tThe term to search for"
  putStrLn "  -h,--help\tShow this help text"

printMsgForMissingArgs :: IO ()
printMsgForMissingArgs = do
  putStrLn "Missing: (-f|--file FILE) (-t|--term TERM)"
  putStrLn "\n"
  printUsageInstructions

printMsgForNoMatchesFound :: SearchTerm -> IO ()
printMsgForNoMatchesFound searchTerm = 
  putStrLn $ "No lines found with the term '" <> searchTerm <> "' present in them!"

{-
  Format:
  ezfindM    [-i]   [-l]   (-f)   (-t)
             flag1  flag2  flag3  flag4

  max args -> flag1 flag2 flag3 fileName flag4 searchTerm -> 6
  min args ->             flag3 fileName flag4 searchTerm -> 4

  cases:

  1 ->    -f fileName -t term
  2 -> -i -f fileName -t term
  3 -> -l -f fileName -t term
  4 -> -i -l -f fileName -t term 

  --------------------------------------------------------------
  
  HOW TO RUN 
  cabal run grepByWord -- "/home/megha/Documents/sample.txt"

  "grepByWord" is the name of the executable as defined under
  the "executable" header in the cabal file.
-}

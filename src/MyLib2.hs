{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}

module MyLib2 where

import Data.Char (toLower)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import System.Console.ANSI

-- Type aliases for flags
type CaseFlag = Bool
type LineNumberFlag = Bool
type HighlightFlag = Bool

type SearchTerm = String 
type Line = (Int, String)

type HLine = (Int, FlaggedTokens)
type FlaggedTokens = [(String, Bool)]
  --where String would be a word, and Bool would indicate info 
  --on whether to highlight this word while printing or not
  
-- IO Functions
demo :: CaseFlag -> LineNumberFlag -> HighlightFlag -> SearchTerm -> FilePath -> IO ()
demo cf lf hf term filePath = do
  lines' <- getLines filePath
  let results = search cf term lines'
  printLines lf hf results

getLines :: FilePath -> IO [Line]
getLines filePath = do
  contents <- readFile filePath
  let lines' = zip [1..] $ lines contents
  pure lines'

printLines :: LineNumberFlag -> HighlightFlag -> [HLine] -> IO ()
printLines lf hf lines' =
  for_ lines' $ \(lineNumber, tokens) -> do
    -- print line number if that flag is True else print a null string
    putStr $ if lf then show lineNumber <> ": " else ""
    if hf 
      then printHLine tokens
      else putStrLn $ (unwords . map fst) tokens

printHLine :: FlaggedTokens -> IO ()
printHLine hLine = do
  for_ hLine $ \(word, highlight) -> do
    let hightlightFn = if highlight then withHighlight else id 
    hightlightFn $ putStr word
    putStr " "
  putStr "\n"

withHighlight :: IO a -> IO a
withHighlight action = do
  setSGR [SetColor Foreground Vivid Green]
  result <- action 
  setSGR [Reset]
  pure result

-- Main search function
search :: CaseFlag -> SearchTerm -> [Line] -> [HLine]
search cf term = mapMaybe (findMatchInLine cf term)  
 
-- Helper functions
findMatchInLine :: CaseFlag -> SearchTerm -> Line -> Maybe HLine
findMatchInLine cf term (lineNumber, lineTxt) = 
  let lineTokens = tokenizeLineWithHFlags lineTxt 
      lineTokens' = searchTerm cf term lineTokens
  in if any snd lineTokens'
      then Just (lineNumber, lineTokens')
      else Nothing

tokenizeLineWithHFlags :: String -> FlaggedTokens
tokenizeLineWithHFlags line = map (, False) (words line)

searchTerm :: CaseFlag -> SearchTerm -> FlaggedTokens -> FlaggedTokens
searchTerm cf term = map $ \(word, hf) ->
  if cf && word == term || not cf && lowercase word == lowercase term
    then (word, True) 
    else (word, hf)

lowercase :: String -> String
lowercase = map toLower

module MyLibIO (
  fetchArgs,
  fetchSearchResult
) where

import System.Console.ANSI
import Data.Foldable (for_)
import qualified Options.Applicative as Options

import MyLib (search)
import Parsers (progParser)
import Types (Args(..), Line, HLine, HTokens) 

fetchArgs :: IO Args
fetchArgs = Options.execParser progParser

fetchSearchResult :: Args -> IO ()
fetchSearchResult (Args ci lf hf filePath term) = do
  lines' <- getLines filePath
  let searchResult = search ci term lines'
  printLines searchResult lf hf

getLines :: FilePath -> IO [Line]
getLines filePath = do
  contents <- readFile filePath
  let lines' = zip [1..] $ lines contents
  pure lines'

printLines :: [HLine] -> Bool -> Bool -> IO ()
printLines lines' lineNumFlag highlightFlag =
  for_ lines' $ \(lineNum, tokens) -> do
    putStr $ if lineNumFlag then show lineNum <> ": " else ""
    printHLine tokens highlightFlag

printHLine :: HTokens -> Bool -> IO ()
printHLine hLine highlightFlag = do
  for_ hLine $ \(word, hTag) -> do
    let hightlightFn = if hTag && highlightFlag then withHighlight else id 
    hightlightFn $ putStr word
    putStr " "
  putStr "\n"

withHighlight :: IO a -> IO a
withHighlight action = do
  setSGR [SetColor Foreground Vivid Green]
  result <- action 
  setSGR [Reset]
  pure result
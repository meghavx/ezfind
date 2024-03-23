module MyLib (
  Line,
  SearchTerm,
  caseSensitiveSearch,
  caseInsensitiveSearch,
  caseSensitiveSearchWithLineNumbers,
  caseInsensitiveSearchWithLineNumbers
) where

import Data.Char (toLower)
import Data.Maybe (mapMaybe)

type Line = String
type SearchTerm = String 

data CaseSensitivityMode = CaseSensitive | CaseInsensitive
  deriving (Eq, Show)

caseSensitiveSearch :: SearchTerm -> [Line] -> [Line]
caseSensitiveSearch term = mapMaybe (findMatch term CaseSensitive)  
   
caseInsensitiveSearch :: SearchTerm -> [Line] -> [Line]
caseInsensitiveSearch term = mapMaybe (findMatch term CaseInsensitive) 

caseSensitiveSearchWithLineNumbers :: SearchTerm -> [Line] -> [Line]
caseSensitiveSearchWithLineNumbers term = caseSensitiveSearch term . prependLineNumbers

caseInsensitiveSearchWithLineNumbers :: SearchTerm -> [Line] -> [Line]
caseInsensitiveSearchWithLineNumbers term = caseInsensitiveSearch term . prependLineNumbers

-- Helper functions
findMatch :: SearchTerm -> CaseSensitivityMode -> Line -> Maybe Line
findMatch term caseSensitivityMode line = 
  let termFound = searchTerm term caseSensitivityMode line :: Bool
  in 
    if termFound
      then Just line
      else Nothing

searchTerm :: SearchTerm -> CaseSensitivityMode -> Line -> Bool
searchTerm term caseSensitivityMode line =
  case caseSensitivityMode of
    CaseSensitive   -> term  `elem` tokens
    CaseInsensitive -> term' `elem` tokens' 
  where 
    tokens  = words line
    term'   = lowercase term
    tokens' = words $ lowercase line

lowercase :: String -> String
lowercase = map toLower

prependLineNumbers :: [Line] -> [Line]
prependLineNumbers = zipWith (\n line -> show n <> ": " <> line) [1..]
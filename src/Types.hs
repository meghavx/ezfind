module Types (
    Args (..),
    Line,
    HLine, 
    HTokens
) where

-- type to store arguments provided by the user
data Args = Args 
  { caseInsensitiveFlag :: Bool
  , lineNumberFlag :: Bool
  , highlightFlag :: Bool
  , file :: FilePath
  , term :: String
} deriving Show

-- types to represent a single line of text

-- Type 1: as a two-tuple consisting of the line number and the line text
type Line = (Int, String)

-- Type 2: as a two-tuple consisting of the line number and a list, 
-- where the list would consist of words from the line paired with 
-- their corresponding highlight tag, which would be a bool value
type HLine = (Int, HTokens)

-- list type used in the HLine type 
type HTokens = [(String, Bool)]
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TupleSections #-}

module MyLib (search) where

import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Types (Line, HLine, HTokens)

search :: Bool -> String -> [Line] -> [HLine]
search ci term = mapMaybe (searchInLine ci term)  

searchInLine :: Bool -> String -> Line -> Maybe HLine
searchInLine ci term (lineNum, lineTxt) = 
  let hLine = tokenizeLineWithHTags lineTxt
      -- if the term is found in hLine, we get back hLine with hTags 
      -- corresponding to words matching the term modified to `True`
      hLine' = searchInHLine ci term hLine

  -- if any pair of word and hTag in HLine, has hTag equal to `True` 
  -- we return this HLine wrapped in Just else we return Nothing
  in if any snd hLine'
      then Just (lineNum, hLine')
      else Nothing

tokenizeLineWithHTags :: String -> HTokens
tokenizeLineWithHTags line = map (, False) (words line)

-- for each pair of word and hTag in HTokens, the word is checked for 
-- equality with term according to the case-insensitivity flag value 
-- and result of the check is updated as the new hTag value in hTokens 
searchInHLine :: Bool -> String -> HTokens -> HTokens
searchInHLine ci term = map $ \(word, hTag) -> 
  (word, ) $ 
    if ci
      then lowercase word == lowercase term
      else word == term 

lowercase :: String -> String
lowercase = map toLower
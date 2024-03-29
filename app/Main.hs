module Main where

import MyLibIO (fetchArgs, fetchSearchResult)

main :: IO ()
main = do
  args <- fetchArgs
  fetchSearchResult args
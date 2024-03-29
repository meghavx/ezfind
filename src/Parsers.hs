module Parsers (progParser) where

import Types (Args(..))
import Control.Applicative ((<**>))
import qualified Options.Applicative as Options

progParser :: Options.ParserInfo Args
progParser = 
  Options.info
    (Args
      <$> caseInsensitiveFlagParser
        <*> lineNumberFlagParser
        <*> highlightFlagParser
        <*> filePathParser
        <*> (searchTermParser <**> Options.helper)) 
    (Options.fullDesc 
      <> Options.header "ezfind - A simpler grep!"
      <> Options.progDesc "Search for a TERM in a text FILE")

caseInsensitiveFlagParser :: Options.Parser Bool
caseInsensitiveFlagParser =
  Options.switch (  
    Options.short 'i'
    <> Options.long "case-insensitive"
    <> Options.help "Set case-insensitive search (disabled by default)")

lineNumberFlagParser :: Options.Parser Bool
lineNumberFlagParser =
  Options.switch (
    Options.short 'l'
    <> Options.long "line-numbers"
    <> Options.help "Print line numbers")

highlightFlagParser :: Options.Parser Bool
highlightFlagParser =
  Options.switch (  
    Options.short 'm'
    <> Options.long "highlight"
    <> Options.help "Highlight term in lines")

filePathParser :: Options.Parser String
filePathParser = 
  Options.option
    Options.str (  
      Options.short 'f'
      <> Options.long "file"
      <> Options.metavar "FILE"
      <> Options.help "Path of the file to search for term in")

searchTermParser :: Options.Parser String
searchTermParser = Options.option
  Options.str (
    Options.short 't'
    <> Options.long "term"
    <> Options.metavar "TERM"
    <> Options.help "The term to search for") 

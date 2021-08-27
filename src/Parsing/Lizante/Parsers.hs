
module Parsing.Lizante.Parsers (
  word
               ) where

import Data.List

import Parsing.Lizante
import Parsing.Lizante.Utils


word :: Parser
word = parse' . words
  where
    parse' []     = Left "No words in input."
    parse' (x:xs) = Right $ ParserOutput (concat . intersperse " " $ xs) Leaf { value = x }

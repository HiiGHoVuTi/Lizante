
module Parsing.Lizante.Parsers (
  word, wordWith,
  literal
               ) where

import Data.List

import Parsing.Lizante
import Parsing.Lizante.Utils


makeWordOutput :: String -> [String] -> RawParserOutput
makeWordOutput x xs = Right $ ParserOutput (concat . intersperse " " $ xs) Leaf { value = x }

word :: Parser
word = parse' . words
  where
    parse' []     = Left "No words in input."
    parse' (x:xs) = makeWordOutput x xs

wordWith :: [Char] -> Parser
wordWith vocab = parse' . words
  where
    parse' []     = Left "No words in input."
    parse' (x:xs)
      | all (flip elem vocab) x = makeWordOutput x xs
      | otherwise = Left "invalid character in word."

literal :: String -> Parser
literal word txt
  | start == word = Right $ ParserOutput end Leaf { value = start }
  | otherwise     = Left "Didn't match literal word."
  where
    start = take (length word) txt
    end   = drop (length word) txt

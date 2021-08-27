
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Parsing.Lizante (

                       ) where

import Data.Either

import Parsing.Lizante.Utils

data ParserOutput = ParserOutput
  { rest :: String
  , post :: Tree String
  }

-- Input -> either <Error> <Output>
type RawParserOutput = Either String (ParserOutput)
type Parser = String -> RawParserOutput

parser :: Parser
parser = const Left "Couldn't match"

-- Setup a node for concatted results
initialResult :: String -> ParserOutput
initialResult s = ParserOutput s treeList

-- Applies a parser over a ParserOutput, Concatenating the results
concatApply :: Parser -> ParserOutput -> RawParserOutput
concatApply f ParserOutput{post = first, ..} = f rest
  >>= \ParserOutput{post = second, ..}
  ->  Right ParserOutput
      { rest = rest
      , post = first >+ second
      }

-- Require both to match in a sequence
(+) :: Parser -> Parser -> Parser
(+) f g x = initialResult x
  |>  concatApply f
  >>= concatApply g

-- Require only the first one to match
(-) :: Parser -> Parser -> Parser
(-) f g x = g x
  |> \case
    Left _  -> f x
    Right _ -> Left "Negative condition hit."

-- Requires either one to match, left priority
(^) :: Parser -> Parser -> Parser
(^) f g x = f x
  |> \case
    Left _ -> g x
    valid  -> valid

-- Utility function for repeated applies
loopApply :: Parser -> RawParserOutput -> RawParserOutput
loopApply f input@(Right v)
  | isRight output = output
  | otherwise      = input
  where
    output = f `concatApply` v
loopApply f e      = e

-- Matches as long as possible, concatenates the result, accepts no match
zeroOrMore :: Parser -> Parser
zeroOrMore f x = loopApply f $ Right (initialResult x)

-- Matches as long as possible, concatenares the result, only accepts one match and more
oneOrMore :: Parser -> Parser
oneOrMore f x = loopApply f $ f x

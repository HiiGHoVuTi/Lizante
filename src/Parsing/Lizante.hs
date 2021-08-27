
{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Parsing.Lizante (

                       ) where

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

-- Applies a parser over a ParserOutput, Concatenating the results
concatApply :: Parser -> ParserOutput -> RawParserOutput
concatApply f ParserOutput{post = first, ..} = f rest
  >>= \ParserOutput{post = second, ..}
  ->  Right ParserOutput
      { rest = rest
      , post = Node { value = "+", children = [ first, second ] }
      }

-- Require both to match in a sequence
(+) :: Parser -> Parser -> Parser
(+) f g x = f x >>= concatApply g

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

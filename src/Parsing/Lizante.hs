
{-# LANGUAGE LambdaCase #-}

module Parsing.Lizante (

                       ) where


(|>) :: a -> (a -> b) -> b
x |> f = f x

type ParserOutput = Either String String

data Parser a = Parser
  { match :: String -> ParserOutput
  , after :: String -> a
  }

parser :: Parser String
parser = Parser
  { match = const$ Left ""
  , after = id
  }

-- Require both to match in a sequence
(+) :: Parser a -> Parser a -> Parser [a]
f + g = Parser
  { match = \ txt
      ->  match f txt
      >>= match g
  , after = \ txt -> [after f txt, after g txt]
  }

-- Require only the first one to match
(-) :: Parser a -> Parser a -> Parser a
f - g = Parser
  { match = \ txt
      -> match g txt
      |> \case
        Left _  -> match f txt
        Right _ -> Right "Negative Condition hit."
  , after = after f
  }

module Main where

import Parsing.Lizante
import Parsing.Lizante.Parsers

main :: IO ()
main = putStrLn . show . getTree . parse $ "hello my name is - -- - Maxime"

getTree (Right (ParserOutput _ o)) = o

parse = group "Main"
        [ zeroOrMore (wordWith "helomyna")
        , word
        , suppress (zeroOrMore (wordWith "-"))
        , oneOrMore (wordWith "Maxime")
        ]

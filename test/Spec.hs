
import Test.HUnit

import Data.Either
import Parsing.Lizante
import Parsing.Lizante.Parsers

ok = TestCase $ assertEqual "this can't show up" 1 1

wordParses   = TestCase . assertBool "Couldn't parse a single word" . isRight $ word "hello world"
wordsParse   = TestCase . assertBool "Couldn't parse a sentence" . isRight $ (oneOrMore word) "hello world"
atLeastOne   = TestCase . assertBool "OneOrMore doesn't fail at 0" . isLeft $ (oneOrMore word) ""
zeroorMore   = TestCase . assertBool "ZeroOrMore fails at 0" . isRight $ (zeroOrMore word) ""
moreThanZero = TestCase . assertBool "ZeroOrMore doesn't match multiple" . isRight $ (zeroOrMore word) "hello world ! I'm maxime"

main = runTestTT $ TestList
  [ TestLabel "OK" ok
  , TestLabel "a single word parses a string of multiple words" wordParses
  , TestLabel "zero or more words can be parsed" wordsParse
  , TestLabel "zero words don't match with one or more" atLeastOne
  , TestLabel "zero or more matches empty string" zeroorMore
  , TestLabel "zero or more still matches multiple words" moreThanZero
  ]

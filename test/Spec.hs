
import Test.HUnit

import Prelude hiding ((+))

import Data.Either
import Parsing.Lizante
import Parsing.Lizante.Utils
import Parsing.Lizante.Parsers

ok = TestCase $ assertEqual "this can't show up" 1 1

getLength (Right (ParserOutput _ (TreeList o))) = length o

wordParses   = TestCase . assertBool "Couldn't parse a single word" . isRight $ word "hello world"
wordsParse   = TestCase . assertBool "Couldn't parse a sentence" . isRight $ (oneOrMore word) "hello world"
concatWorks  = TestCase . assertEqual "Concat doesn't apply both patterns" 2 . getLength $ (word + word) "hello world"
atLeastOne   = TestCase . assertBool "OneOrMore doesn't fail at 0" . isLeft $ (oneOrMore word) ""
zeroorMore   = TestCase . assertBool "ZeroOrMore fails at 0" . isRight $ (zeroOrMore word) ""
moreThanZero = TestCase . assertBool "ZeroOrMore doesn't match multiple" . isRight $ (zeroOrMore word) "hello world ! I'm maxime"
allElems     = TestCase . assertEqual "ZeroOrMore doesn't catch all words" 5 . getLength $ (zeroOrMore word) "hello my name is Maxime"
wordsWith    = TestCase . assertBool "can't create a word with specific chars" . isRight $ (wordWith "ba") "aaabababa"
wordsNotWith = TestCase . assertBool "accepts illegal characters" . isLeft $ (wordWith "ab") "ababbacababab"
groupConcat  = TestCase . assertBool "group parses the whole sequence" . isRight $ (group "My Group"
                                                                                    [ wordWith "ab"
                                                                                    , wordWith "dae"
                                                                                    , wordWith "beo"]) "ababa dade bebo"
suppChildren = TestCase . assertEqual "Suppress doesn't actually delete children" 0 . getLength $ suppress (zeroOrMore word) "hello my name is Maxime"
literally    = TestCase . assertBool "literal doesn't match what it is supposed to" . isRight $ (literal "Maxime") "Maxime is awesome"
litchally    = TestCase . assertBool "literal doesn't match similar strings" . isLeft $ (literal "Maxime") "Moxime is really awful"
ignored      = TestCase . assertEqual "characters aren't ignored" 2 . getLength $ (zeroOrMore (ignore (literal"-") word)) "----hello --world"
edgecase1    = TestCase . assertEqual "maybe consider adding this ?" 2 . getLength $ (zeroOrMore (ignore (literal"-") word)) "----hello--world"
optionally   = TestCase . assertBool "optional doesn't allow a skip" . isRight $ (optional whitespace + word) "Maxime"
striteral    = TestCase . assertBool "literal' isn't strict enough" . isLeft $ (literal' "Maxime") " Maxime "
worditeral   = TestCase . assertBool "literal isn't permissive enough" . isRight $ (literal "Maxime") "   Maxime "


main = runTestTT $ TestList
  [ TestLabel "OK" ok
  , TestLabel "a single word parses a string of multiple words" wordParses
  , TestLabel "zero or more words can be parsed" wordsParse
  , TestLabel "the + operator works" concatWorks
  , TestLabel "zero words don't match with one or more" atLeastOne
  , TestLabel "zero or more matches empty string" zeroorMore
  , TestLabel "zero or more still matches multiple words" moreThanZero
  , TestLabel "zero or more matches all the words" allElems
  , TestLabel "words can be created" wordsWith
  , TestLabel "words only tolerate restricted letters" wordsNotWith
  , TestLabel "group does apply the whole sequence" groupConcat
  , TestLabel "suppress does delete the children" suppChildren
  , TestLabel "literal works as expected" literally
  , TestLabel "literal works as expected" litchally
  , TestLabel "ignore does ignore characters" ignored
  , TestLabel "optional skips without breaking" optionally
  , TestLabel "literal' is strict" striteral
  , TestLabel "literal ignores whitespace" worditeral
  -- , TestLabel "ignore works with word" edgecase1
  ]

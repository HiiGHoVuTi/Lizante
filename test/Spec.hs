
import Test.HUnit

ok = TestCase $ assertEqual "this can't show up" 1 1

main = runTestTT $ TestList
  [ TestLabel "OK" ok
  ]

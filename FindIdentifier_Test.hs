-- | Testing FindIdentifier

module FindIdentifier_Test where

import FindIdentifier( findIdentifier )
import Test.HUnit

borderCases =  TestLabel "Border test cases" (TestList [
              testEmpty, testNegCursor, testComment
            ] )

testEmpty = TestCase $ assertEqual
  "Should get Nothing form an empty string"  Nothing  (findIdentifier "" (1,1))
testNegCursor = TestCase $ assertEqual
  "Should get Nothing when cursor is nagative" Nothing (findIdentifier "a" (-1, -1))
testComment = TestCase $ assertEqual
  "Should get Nothing on comment" Nothing (findIdentifier "-- a" (1,3))


simpleCases = TestLabel "Simple, but serious cases" ( TestList [
     testMinimal
  ])

testMinimal = TestCase $ assertEqual
  "Minimal program" (Just "main") (findIdentifier "main = print" (1,2))


main = runTestTT $ TestList[borderCases, simpleCases]

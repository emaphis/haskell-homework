-- | Addition currying examples

module  Addition where


-- a function that takes three parametes
-- and returns a value
addThreeInputs :: Int -> Int -> Int -> Int
addThreeInputs a b c  = a + b + c


-- passing one parameter to a function that takes three paremeters
-- returns a function that takes two parameters and returns a value
add1ToTwoInputs :: Int -> Int -> Int
add1ToTwoInputs  = addThreeInputs 1
-- or
-- add1Totwoinputs b c = addThreeinputs 1 b c


-- passing one parameter to a function that takes two paremeters
-- returns a function that takes one parameter and returns a value
add3ToOneInput :: Int -> Int
add3ToOneInput  = add1ToTwoInputs 2
-- or
-- add3ToOneInput c = add1ToTwoInputs 2 c


-- a constant that doesn't take any parameters
six :: Int
six  = add3ToOneInput 3


-- test the functions
testAdd :: Bool
testAdd =
  addThreeInputs 1 2 3  == 6 &&
  add1ToTwoInputs  2 3  == 6 &&
  add3ToOneInput     3  == 6 &&
  six == 6

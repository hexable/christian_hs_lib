{- |
Module      :  <christian_lib.hs>
Description :  <My problems solved in Haskell>

Maintainer  :  <ctfgm@prtonmail.ch>
Stability   :  unstable
Portability :  portable

<The foundations for solving any bigger problem. Note that any conflicting
function name is postfixed with '2'.>
-}

import Data.Char

-- Exclusive or
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False

-- Are three Ints equal
areThreeIntsEqual :: Int -> Int -> Int -> Bool
areThreeIntsEqual a b c = (a == b) && (b == c)

-- Are three Ints not equal
areThreeIntsNotEqual :: Int -> Int -> Int -> Bool
areThreeIntsNotEqual a b c = (a /= b) && (a /= c) && (b /= c)

-- Max of two Ints
max2 :: Int -> Int -> Int
max2 x y
  | x < y = y
  | otherwise = x

-- Max of two Ints operator
(&&&) :: Int -> Int -> Int
x &&& y = max2 x y

--  Max of three Ints
maxThree :: Int -> Int -> Int -> Int
maxThree x y z = max2 (max2 x y) z

-- Max of four Ints
maxFour :: Int -> Int -> Int -> Int -> Int
maxFour x y z w = max2 (maxThree x y z) w

-- Is an Int between two other Ints
between :: Int -> Int -> Int -> Bool
between x y z
  | (y <= x) && (x <= z) = True
  | (z <= x) && (x <= y) = True
  | otherwise = False

-- Does a sequence not decrease
weakAscendingOrder :: Int -> Int -> Int -> Bool
weakAscendingOrder x y z
  | (x <= y) && (y <= z) = True
  | otherwise = False

-- Is an Int between two other Ints
between2 :: Int -> Int -> Int -> Bool
between2 x y z = weakAscendingOrder y x z

-- !&&
nAnd :: Bool -> Bool -> Bool
nAnd x y = not(x && y)

-- Second implementation of nAnd
nAnd2 :: Bool -> Bool -> Bool
nAnd2 True False = True
nAnd2 False True = True
nAnd2 False False = True
nAnd2 True True = False

-- Char from ['0','Z'] to respective Int where 'A' = 10
charDigitToInt :: Char -> Int
charDigitToInt x = digitToInt x

-- Is a Char lowercase
isLower2 :: Char -> Bool
isLower2 c
  | ('a' <= c) && (c <= 'z') = True
  | otherwise = False

-- Is a Char uppercase
isUpper2 :: Char -> Bool
isUpper2 c
  | ('A' <= c) && (c <= 'Z') = True
  | otherwise = False

-- Offset between uppercase and lowercase
capitalOffset :: Int
capitalOffset = ord 'a'  - ord 'A'

-- Convert lowercase Char to uppercase Char
toUpper2 :: Char -> Char
toUpper2 c
  | isUpper2 c = c
  | otherwise = chr((ord c) - capitalOffset)

-- Convert uppercase Char to lowercase Char
toLower2 :: Char -> Char
toLower2 c
  | isLower2 c = c
  | otherwise = chr((ord c) + capitalOffset)

-- Is a character in the range ['0', '9']
isCharDigit :: Char -> Bool
isCharDigit c
  | ('0' <= c) && (c <= '9') = True
  | otherwise = False

-- Are two integers equal, 1 for yay, 0 for nay
equalToInt :: Int -> Int -> Int
equalToInt x y
  | x == y = 1
  | x /= y = 0

-- How many of three Ints are equal
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y = 2 + (equalToInt y z)
  | y == z = 2
  | x == z = 2
  | otherwise = 0

-- Factorial
fac :: Int -> Int
fac n
  | n == 0 = 1
  | n > 0 = n * fac (n - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

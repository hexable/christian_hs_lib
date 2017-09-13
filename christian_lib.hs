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

-- Are three Ints equal
are3IntsEqual :: Int -> Int -> Int -> Bool
are3IntsEqual a b c = (a == b) && (b == c)

-- Are three Ints not equal
are3IntsNotEqual :: Int -> Int -> Int -> Bool
are3IntsNotEqual a b c = (a /= b) && (a /= c) && (b /= c)

-- Max of two Ints
max2 :: Int -> Int -> Int
max2 x y
  | x < y = y
  | otherwise = x

-- Max of two Ints operator
(&&&) :: Int -> Int -> Int
x &&& y = max2 x y

--  Max of 3 Ints
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 (max2 x y) z

-- Max of 4 Ints
max4 :: Int -> Int -> Int -> Int -> Int
max4 x y z w = max2 (max3 x y z) w

-- Min of 2 Ints
min2 :: Int -> Int -> Int
min2 x y
  | x < y = x
  | otherwise = y

-- Min of 3 Ints
min3 :: Int -> Int -> Int -> Int
min3 x y z = min2 (min2 x y) z

-- Min of 4 Ints
min4 :: Int -> Int -> Int -> Int -> Int
min4 x y z w = min2 (min3 x y z) w

-- Middle of 3 Ints
middle3 :: Int -> Int -> Int -> Int
middle3 x y z
  | x == min3 x y z = min2 y z
  | y == min3 x y z = min2 x z
  | z == min3 x y z = min2 x y

-- Is an Int between two other Ints
between :: Int -> Int -> Int -> Bool
between x y z
  | (y <= x) && (x <= z) = True
  | (z <= x) && (x <= y) = True
  | otherwise = False

-- Is there a weak ascending order
weakAscOrder :: Int -> Int -> Int -> Bool
weakAscOrder x y z
  | (x <= y) && (y <= z) = True
  | otherwise = False

-- Between using weak ascending order
between2 :: Int -> Int -> Int -> Bool
between2 x y z = weakAscOrder y x z

-- !&&
nand :: Bool -> Bool -> Bool
nand x y = not(x && y)

-- Second implementation of nand
nand2 :: Bool -> Bool -> Bool
nand2 True False = True
nand2 False True = True
nand2 False False = True
nand2 True True = False

-- Exclusive or
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor False False = False
xor True True = False

-- Second implementation of xor
xor2 :: Bool -> Bool -> Bool
xor2 p q = not(p && q) && (p || q)

-- Char from range ['0','Z'] to respective Int where 'A' = 10
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
uppercaseOffset :: Int
uppercaseOffset = ord 'a'  - ord 'A'

-- Convert lowercase Char to uppercase Char
toUpper2 :: Char -> Char
toUpper2 c
  | isUpper2 c = c
  | otherwise = chr((ord c) - uppercaseOffset)

-- Convert uppercase Char to lowercase Char
toLower2 :: Char -> Char
toLower2 c
  | isLower2 c = c
  | otherwise = chr((ord c) + uppercaseOffset)

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
howManyOf3IntsEqual :: Int -> Int -> Int -> Int
howManyOf3IntsEqual x y z
  | x == y = 2 + (equalToInt y z)
  | y == z = 2
  | x == z = 2
  | otherwise = 0

-- What of 3 Ints are equal
whatOf3IntsEqual :: Int -> Int -> Int -> [Int]
whatOf3IntsEqual x y z
  | x == y = [x]
  | y == z = [z]
  | x == z = [x]
  | otherwise = []

-- Factorial of natural numbers
fac :: Int -> Int
fac x
  | 0 == x = 1
  | 0 < x = x * fac (x - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Range product. With range [m,n], it gives
-- m * (m + 1) * ... * (n - 1) * n
rangeProduct :: Int -> Int -> Int
rangeProduct m n
  | n < m = 0
  | n == m = 1
  | otherwise = n * rangeProduct m (n - 1)

-- Factorial of natural numbers using range product
fac2 :: Int -> Int
fac2 x
  | x == 0 = 1
  | 0 < x = rangeProduct 1 x
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Powers of 2 with natural numbers
power2 :: Int -> Int
power2 x
  | 0 == x = 1
  | 0 < x = 2 * power2 (x - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Sum factorials of natural numbers so
-- sumFacs x = fac 0 + .. + fac (x - 1) + fac x
sumFacs :: Int -> Int
sumFacs x
  | 0 == x = fac 0
  | 0 < x = fac x + sumFacs (x - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Sum function, f, of Int -> Int using natural numbers so
-- sumIntFunction f x = f 0 + .. + f (x - 1) + f x
sumIntFunction :: (Int -> Int) -> Int -> Int
sumIntFunction f x
  | 0 == x = f 0
  | 0 < x = f x + sumIntFunction f (x - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Multiplication of natural numbers using addition
mult :: Int -> Int -> Int
mult x y
  | (0 == x)  || (0 == y) = 0
  | 0 < x = x + mult x (y - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Integer square root of natural numbers
primitiveIntSqrt :: Int -> Int -> Int
primitiveIntSqrt x y
  | x < 0 = error "ERROR(christian): Input not a natural number"
  | (y * y) == x = y
  | x < (y * y) = y - 1
  | otherwise = primitiveIntSqrt x (y + 1)

-- Determines the max Int output of an Int -> Int function applied to a range
-- [0,n]. In other words, taking the max of the set {f 0, .. , f (n - 1), f n}
maxIntFunction :: (Int -> Int) -> Int -> Int
maxIntFunction f n
  | n == 0 = f 0
  | 0 < n = max2 (f n) (f (n - 1))
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Calculate the nth Fibbonaci number, see my faster version below :-)
fib :: Int -> Int
fib n
  | 0 == n = 0
  | 1 == n = 1
  | 1 < n = fib (n - 2) + fib (n - 1)
  | otherwise = error "ERROR(christian): Input not a natural number or div by 0"

-- Remainder after division of real numbers
remainder :: Int -> Int -> Int
remainder num denom
  | (0 <= num) && (num < denom) = num
  | (denom < num) && (0 < denom) = remainder (num - denom) denom
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Quotient after division of real numbers. AKA integer division
intDivide :: Int -> Int -> Int
intDivide num denom
  | num < denom = 0
  | denom <= num && (0 < denom) = 1 + intDivide (num - denom) denom
  | otherwise = error "ERROR(christian): Input not a natural number or div by 0"

-- TODO(christian): GCD & exponent optimised

-- Return the min & max Int of 2 Ints
getMinAndMax :: Int -> Int -> (Int, Int)
getMinAndMax x y
  | x < y = (x, y)
  | y < x = (y, x)
  | otherwise = (x, y)

-- How many times do we see an Int
howManyOccursIntIn2 :: Int -> Int -> Int -> Int
howManyOccursIntIn2 w x y = (w `equalToInt` x) + (w `equalToInt` y)

howManyOccursIntIn3 ::  Int -> Int -> Int -> Int -> Int
howManyOccursIntIn3 w x y z
  = (w `equalToInt` x) + (w `equalToInt` y)  + (w `equalToInt` z)

-- Example tuple work
shiftTupleRightExample :: ((Int, Int), Int) -> (Int, (Int, Int))
shiftTupleRightExample ((x, y), z) = (z, (x, y))

-- Add pair using haskell selector functions
addTupleIntPair :: (Int, Int) -> Int
addTupleIntPair s = fst s + snd s

-- nth Fibonacci number with tuples!
fibStep :: (Int, Int) -> (Int, Int)
fibStep (u, v) = (v, u + v)

nthFibPair :: Int -> (Int, Int)
nthFibPair n
  | 0 == n = (0, 1)
  | 0 < n = fibStep (nthFibPair (n - 1))
  | otherwise = error "ERROR(christian): Input not a natural number"

fib2 :: Int -> Int
fib2 = fst . nthFibPair

-- Max Int and how many times we see it amongst two Ints
getMaxAndOccursIn2 :: Int -> Int -> (Int, Int)
getMaxAndOccursIn2 x y
  | x == y = (x, 2)
  | otherwise = ((max2 x y), 1)

getMaxAndOccursIn3 :: Int -> Int -> Int -> (Int, Int)
getMaxAndOccursIn3 x y z = ((max3 x y z),
                            (howManyOccursIntIn3 (max3 x y z) x y z))

-- Order triple
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (p, q, r) = (max3 p q r, middle3 p q r, min3 p q r)

-- Find the x-axis intersection of a straight line on a cartesian plane.
-- Expect results of infinity if you pass a line that doesn't corss the x-axis.
-- m == gradient
-- c == y-axis intersection
type Coord = (Float, Float)
xAxisIntersect :: Float -> Float -> Coord
xAxisIntersect m c
  | c == 0 = (0, 0)
  | otherwise = (((-c) / m), 0)

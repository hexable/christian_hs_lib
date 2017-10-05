-----------------------------------------------------------------------------
-- |
-- Module      :  christian_lib
-- Maintainer  :  ctfgm@protonmail.ch
-- Stability   :  unstable
-- Portability :  portable
--
-- This is my personal haskell library, let's see where it goes.
--
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Study notes --

-- Primitive recursion always terminates.

{- "Haskell functions can take functions as parameters and return functions as
return values. A function that does either of those is called a higher order
function." ~ learnyouahaskell
-}

-- Function application is left associative. e.g f a b c == (((f a) b) c)

-- -> is right associative. e.g a -> b -> c == (a -> (b -> c))

{- ($) has the lowest precedence, where ($) :: (a -> b) -> a -> b. So b is
always evaluated first
-}

{- (.) is right associative. e.g (f.g.z) x == f (g (z x))
-}

-----------------------------------------------------------------------------

import Test.QuickCheck
import Data.Char
import Data.List

{- Here is your machine's Int boundaries. You are at least guaranteed
[-(2^29), 2^29].
-}
minIntBound :: Int
minIntBound = minBound

maxIntBound :: Int
maxIntBound = maxBound

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
weakAscOrder x y z = (x <= y) && (y <= z)

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
isLower2 c = ('a' <= c) && (c <= 'z')

-- Is a Char uppercase
isUpper2 :: Char -> Bool
isUpper2 c = ('A' <= c) && (c <= 'Z')

-- Now with currying!
isUpper3 :: Char -> Bool
isUpper3 = (`elem` ['A'..'Z'])

-- Offset between uppercase and lowercase
uppercaseOffset :: Int
uppercaseOffset = ord 'a'  - ord 'A'

-- Convert lowercase Char to uppercase Char
toUpper2 :: Char -> Char
toUpper2 c
  | isUpper2 c || not(isAlpha c) = c
  | otherwise = chr((ord c) - uppercaseOffset)

-- Convert uppercase Char to lowercase Char
toLower2 :: Char -> Char
toLower2 c
  | isLower2 c || not(isAlpha c) = c
  | otherwise = chr((ord c) + uppercaseOffset)

-- Is a character in the range ['0', '9']
isCharDigit :: Char -> Bool
isCharDigit c = ('0' <= c) && (c <= '9')

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

{- Range product. With range [m,n], it gives
m * (m + 1) * ... * (n - 1) * n
-}
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

{- Sum factorials of natural numbers so
sumFacs x = fac 0 + .. + fac (x - 1) + fac x
-}
sumFacs :: Int -> Int
sumFacs x
  | 0 == x = fac 0
  | 0 < x = fac x + sumFacs (x - 1)
  | otherwise = error "ERROR(christian): Input not a natural number"

{- Sum function, f, of Int -> Int using natural numbers so
sumIntFunction f x = f 0 + .. + f (x - 1) + f x
-}
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

{- Determines the max Int output of an Int -> Int function applied to a range
[0,n]. In other words, taking the max of the set {f 0, .. , f (n - 1), f n}
-}
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
intDivide :: Integer -> Integer -> Integer
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

{- Find the x-axis intersection of a straight line on a cartesian plane.
Expect results of infinity if you pass a line that doesn't corss the x-axis.
m == gradient
c == y-axis intersection
-}
type Coord = (Float, Float)
xAxisIntersect :: Float -> Float -> Coord
xAxisIntersect m c
  | c == 0 = (0, 0)
  | otherwise = (((-c) / m), 0)

-- Is even
isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- Is odd
isOdd :: Int -> Bool
isOdd x = not(isEven x)

-- Testing the <- generator
sumListTuples :: [(Int, Int)] -> [Int]
sumListTuples list = [m + n | (m,n) <- list]

-- Get all digits in a String
stringToDigitString :: String -> String
stringToDigitString string = [c | c <- string, isCharDigit c]

-- Are all elements of a list even
allEven :: [Int] -> Bool
allEven list = list == [s | s <- list, isEven s]

-- Are all elements of a list odd
allOdd :: [Int] -> Bool
allOdd list = [] == [s | s <- list, isEven s]

-- Double all elements of a list
doubleList :: [Int] -> [Int]
doubleList list = [2 * s | s <- list]

-- Capitalise any Char in a String
capitalise :: String -> String
capitalise string = [toUpper2 s | s <- string]

-- Capitalise but now only return alpha Chars
capitaliseLetters :: String -> String
capitaliseLetters string = [toUpper2 s | s <- string, 'A' <= s, s <= 'z']

-- Factors of a natural number
factors :: Int -> [Int]
factors x = [s | s <- [1 .. div x 2], (mod x s) == 0] ++ [x]

-- Is a natural number prime
isPrime :: Int -> Bool
isPrime x
  | 0 <= x = [1, x] == factors x
  | otherwise = error "ERROR(christian): Input not a natural number"

-- Get the first element of a list
head2 :: [q] -> q
head2 (q : _) = q
head2 [] = error "ERROR(christian): Input is an empty list"

-- Get everything after the head of a list
tail2 :: [q] -> [q]
tail2 (_ : qs) = qs
tail2 [] = error "ERROR(christian): Input is an empty list"

-- Is an alpha Char?
isAlpha2 :: Char -> Bool
isAlpha2 c = ('A' <= c) && (c <= 'z')

-- Is white space?
isWhiteSpace :: Char -> Bool
isWhiteSpace c = c == ' '

-- Not white space?
notWhiteSpace :: Char -> Bool
notWhiteSpace c = c /= ' '

{- Get elements in a list while a predicate function, p, is satisfied, reading
list from left to right.
-}
takeWhilePredicate :: (q -> Bool) -> [q] -> [q]
takeWhilePredicate p [] = []
takeWhilePredicate p (q : qs)
  | p q = q : takeWhilePredicate p qs
  | otherwise = []

-- Get the suffix that takeWhilePredicate drops
dropWhilePredicate :: (q -> Bool) -> [q] -> [q]
dropWhilePredicate p [] = []
dropWhilePredicate p (q : qs)
  | p q = dropWhilePredicate p qs
  | otherwise = qs

-- Erase leading white space
eraseLeadingWhiteSpace :: String -> String
eraseLeadingWhiteSpace [] = []
eraseLeadingWhiteSpace (c : cs)
  | notWhiteSpace c = (c : cs)
  | isWhiteSpace c = eraseLeadingWhiteSpace cs

{- Take a tuple with the first part as a sentence and the second part an empty
list. The return is another tuple with the first part as the sentence minus the
first word and any following whtie space. The second part is the first word.
-}
firstWord :: (String, String) -> (String, String)
firstWord ([], []) = ([], [])
firstWord ([], words) = ([], words)
firstWord ((c : cs), word)
  | isWhiteSpace c = (eraseLeadingWhiteSpace cs, word)
  | notWhiteSpace c = firstWord (cs, word ++ [c])

{- Break a String in a list of the words. Words are seperated by white space.
There already exists a Prelude function but this is my own try at an
implementation. Doing a quickCheck comparison of mine vs Prelude's revealed I
don't know how to return [] instead of [""] when " " is passed as an argument.
-}
stringToWords :: String  -> [String]
stringToWords [] = []
stringToWords cs = wordTaken ++ rest
  where wordTaken = [(snd (firstWord (cs, [])))]
        rest = (stringToWords (fst (firstWord (cs, []))))

-- Reverse polish notation arithmetic of +, -, * and div over natural numbers
rpn :: String -> Integer
rpn string
  = let parsedString = words string

        foldStackFunction (x:y:rest) "*" = (x*y):rest
        foldStackFunction (x:y:rest) "/" = (intDivide y x):rest
        foldStackFunction (x:y:rest) "+" = (x+y):rest
        foldStackFunction (x:y:rest) "-" = (y-x):rest
        foldStackFunction stack number = (read number):stack

    in head $ foldl foldStackFunction [] parsedString

-- Pattern matching function that adds the first two Integers in a list
sumHeads :: [Integer] -> Integer
sumHeads (x:y:_) = x + y
sumHeads (x:_) = x
sumHeads _ = 0

-- && over a list of Bools
listAnd :: [Bool] -> Bool
listAnd (x:xs)
  | xs == [] = x
  | otherwise = x && listAnd xs
listAnd [] = error "ERROR(christian): Input is an empty list"

listAnd2 :: [Bool] -> Bool
listAnd2 (x:xs) = x && (listAnd2 xs)
listAnd2 _ = True -- Note the preservation of &&'ing over an empty set

listAnd3 :: [Bool] -> Bool
listAnd3 xs = foldr (&&) True xs

listAnd4 :: [Bool] -> Bool
listAnd4 xs = not (False `elem` xs)

listAnd5 :: [Bool] -> Bool
listAnd5 xs = all id xs

-- lor (||) over a list of Bools
listOr :: [Bool] -> Bool
listOr xs = True `elem` xs

listOr2 :: [Bool] -> Bool
listOr2 xs = any id xs

{- How many times an Integer occurs in list. Note that using genericLength gives
the extra precision of Integer with the penalty of cost for computation. Both
Integer and Int versions are available.
-}
integerListOccurs :: Integer -> [Integer] -> Integer
integerListOccurs x xs = genericLength [occ | occ <- xs, occ == x]

intListOccurs :: Int -> [Int] -> Int
intListOccurs x xs = length [occ | occ <- xs, occ == x]

intListOccurs2 :: Int -> [Int] -> Int
intListOccurs2 x (y:ys) = (equalToInt x y) + (intListOccurs2 x ys)
intListOccurs2 _ _ = 0

intListOccurs3 :: Int -> [Int] -> Int
intListOccurs3 x xs = length $ filter (== x) xs


-- Get any Integers that only appear once
uniqueInteger :: [Integer] -> [Integer]
uniqueInteger ns = [xs | xs <- ns, (integerListOccurs xs ns) == 1]

{- e.g "ship" is a substring of "fish and chips"
but not "hippies"
-}
isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring p@(x:xs) q@(y:ys)
  | x == y = isSubstring xs ys
  | otherwise = isSubstring p ys

{- e.g "chip" is a subsequence of "fish and chips"
but not "chin up"
-}
isSubsequence :: String -> String -> Bool
isSubsequence s t = helper s t 0
  where helper :: String -> String -> Int -> Bool
        helper [] _ _ = True
        helper _ [] _ = False
        helper p@(x:xs) q@(y:ys) flag
          | x == y = helper xs ys 1
          | (x /= y) && (flag /= 1) = helper p ys flag
          | otherwise = False

-- Thanks Niek for this one ;)
isSubsequence2 :: String -> String -> Bool
isSubsequence2 [] _ = True
isSubsequence2 _ [] = False
isSubsequence2 (x:xs) (y:ys)
  | x == y && helper xs ys = True
  | otherwise = isSubsequence2 (x:xs) ys
  where
    helper :: String -> String -> Bool
    helper [] _ = True
    helper _ [] = False
    helper (x:xs) (y:ys)
      | x == y && helper xs ys = True
      | otherwise = False

-- Is a String a palindrome?
isPalindrome :: String -> Bool
isPalindrome cs = cleanString == reverseCleanString
  where cleanString = map toLower (filter isAlpha2 cs)
        reverseCleanString = reverse cleanString

-- Make a list of Strings of all digits in String
stringDigits :: String -> [String]
stringDigits digits = [[c] | c <- digits, isCharDigit c]

-- Make a list of strings of each digit in an Integer
integerToDigitStringList :: Integer -> [String]
integerToDigitStringList x = stringDigits $ show x

{- Modular exponentiation. This is the calculation of 'x^n mod m'. For example
you might want the last few digits in a crazy large computation of x^n.
-}
modularExpo :: Integer -> Integer -> Integer -> Integer
modularExpo x n m = modularExpoHelper x n m 1 where
  modularExpoHelper x n m product
    | n == 0 = product
    | even n = modularExpoHelper (mod (x*x) m) (div n 2) m product
    | odd n
    = modularExpoHelper (mod (x*x) m) (div n 2) m (mod (product*x) m)

{- Modular exponentiation, where d is the number of digits at the end of the
result you want
-}
lastDigitsPow :: Integer -> Integer -> Integer -> Integer
lastDigitsPow x n 0 = error "ERROR(christian): digit number request is 0"
lastDigitsPow x n d = modularExpo x n (10^d)

-- Computes  x^n + x^(n-1) + .. + x^0 and gives the last d digits
lastDigitsPowSum :: Integer -> Integer -> Integer -> Integer -> Integer
lastDigitsPowSum x n 0 sum = error "ERROR(christian): digit number request is 0"
lastDigitsPowSum x 0 d sum = mod (sum + 1) (10^d)
lastDigitsPowSum x n d sum
  = lastDigitsPowSum x (n-1) d (sum + (lastDigitsPow x n d))

-- Append n 0's to a list
append0 :: Integer -> [Integer] -> [Integer]
append0 0 xs = xs
append0 n xs = append0 (n-1) (0:xs)

-- Preforms lastDigitsPowSum and returns each digit, in order, in a list.
lastDigitsPowSumList :: Integer -> Integer -> [Integer]
lastDigitsPowSumList n 0 = []
lastDigitsPowSumList n d = append0 lengthDiff ds
  where cs = integerToDigitStringList $ lastDigitsPowSum n n d 0
        ds = [read digit | digit <- cs]
        lengthDiff = d - genericLength ds

-- Polynomial addition, which is associative!
polyAdd :: [Double] -> [Double] -> [Double]
polyAdd [] [] = []
polyAdd a b
  | aDeg == bDeg = dropWhile (==0) $ zipWith (+) a b
  | bDeg < aDeg =  dropWhile (==0) $ (take (aDeg - bDeg) a) ++ zipResult
  | aDeg < bDeg =  dropWhile (==0) $ (take (bDeg - aDeg) b) ++ zipResult
  where aDeg = genericLength a
        bDeg = genericLength b
        zipResult = reverse $ zipWith (+) (reverse a) (reverse b)

-- Negate a Double unless it's 0
negDouble :: Double -> Double
negDouble x = if x == 0.0 then 0.0 else -x

-- Polynomial subtraction. Just like arithmetic subtraction, not associative
polySub :: [Double] -> [Double] -> [Double]
polySub a b = polyAdd a (map (negDouble) b)

-- Polynomial multiplication by constant
polyMultiply :: [Double] -> [Double] -> [Double]
polyMultiply [] _ = []
polyMultiply _ [] = []
polyMultiply [c] ys = dropWhile (==0) (map (*c) ys)
polyMultiply (x:xs) ys =
  dropWhile (==0) $ polyAdd ((map (*x) ys) ++ [0.0]) (polyMultiply xs ys)

-- Polynomial, leading term division
polyLeadDiv :: [Double] -> [Double] -> [Double]
polyLeadDiv _ [] = error "ERROR(christian): division by 0"
polyLeadDiv [] _ = []
polyLeadDiv a b = [coeffDiv] ++ (replicate degDiff 0.0)
  where aDeg = (genericLength a)-1
        bDeg = (genericLength b)-1
        degDiff = aDeg - bDeg
        coeffDiv = (head a) / (head b)

-- Polynomial division
polDivision :: [Double] -> [Double] -> ([Double], [Double])
polDivision _ [] = error "ERROR(christian): Divisor is 0"
polDivision [] _ = ([], [])
polDivision num denom =
  let quotient = []
      remainder = num
  in alg num denom remainder quotient where

    alg :: [Double] -> [Double] -> [Double] -> [Double] -> ([Double], [Double])
    alg n d r q
      | (r == []) || (rDeg < dDeg) = (q, r)
      | otherwise = alg n d r' q'
      where rDeg = (genericLength r)-1
            dDeg = (genericLength d)-1
            qTerm = polyLeadDiv r d
            q' = polyAdd q qTerm
            r' = polySub r (polyMultiply qTerm d)

-- List up to the nth Triangular number
triangularNumbers :: Integer -> [Integer]
triangularNumbers n = scanl (+) 1 [2..n]

{- Create infinite Run-length sequence.
Example sequence: 1,2,2,1,1,2,...
-}
--TODO

{- Decode a run-length sequence.
Example: 1,2,2,1,1,2,1,... <- 1,2,2,1,1,...
-}
decodeRunlengthSequence :: [Int] -> [Int]
decodeRunlengthSequence x = helper x 0 0
  where helper :: [Int] -> Int -> Int -> [Int]
        helper [x] _ _ = [1]
        helper p@(x:xs) digit count
          | count == 0 = helper xs x 1
          | (count == 1) && (x /= digit) = [1] ++ helper p 0 0
          | otherwise = [2] ++ helper xs 0 0

-- Implementation of map using list comprehension
map2 :: (a -> a) -> [a] -> [a]
map2 f xs = [f x | x <- xs]

-- Implementation of map using recursion
map3 :: (a -> a) -> [a] -> [a]
map3 f [] = []
map3 f (x:xs) = (f x) : (map3 f xs)

-- Implementation of filter using list comprehension
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = [x | x <- xs, f x]

-- Implementation of filter using recursion
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f [] = []
filter3 f (x:xs)
  | f x = x:(filter3 f xs)
  | otherwise = filter3 f xs

-- Implementation of recursive foldr
foldr2 :: (a -> a -> a) -> a -> [a] -> a
foldr2 f z [] = z
foldr2 f z (x:xs) = x `f` foldr2 f z xs

-- Implementation of recursive foldl
foldl2 :: (a -> a -> a) -> a -> [a] -> a
foldl2 f z xs = foldr2 f z (reverse xs)

-- Implementation of ++
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
[] +++ ys = ys
(x:xs) +++ ys = x:(xs +++ ys)

{- Are my implementations correct? Note the need of substituting function
variables
-}
prop_map2_times xs = map2 (*2) xs == map (*2) xs
prop_map3_times xs = map2 (*2) xs == map (*2) xs

prop_filter2_even xs = filter2 isEven xs == filter isEven xs
prop_filter3_even xs = filter3 isEven xs == filter isEven xs

prop_foldr2_sum xs = foldr2 (+) 0 xs == foldr (+) 0 xs
prop_foldl2_sum xs = foldl2 (+) 0 xs == foldl (+) 0 xs

prop_conc xs ys = (+++) xs ys == (++) xs ys

-- ++'ing over lists in a list
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs:xss) = xs ++ concat xss

-- Mutual recursion of even/odd
isEven2 :: Integer -> Bool
isEven2 x = x == 0 || (0 < x && isOdd2 (x-1)) || (x < 0 && isOdd2 (x+1))

isOdd2 :: Integer -> Bool
isOdd2 x = (0 < x && isEven2 (x-1) || x < 0 && isEven2 (x+1))

-- Insert element in ascending ordered list
insert2 :: Ord a => a -> [a] -> [a]
insert2 x [] = [x]
insert2 x (y:ys)
  | x < y = x:y:ys
  | otherwise = y:(insert x ys)

-- Insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert2 x (insertionSort xs)

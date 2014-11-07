{-
  Name: Simon Baird
  Collaborators:
  Notes:
-}
module HW01 where -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

--
-- Exercise 1 (first version)
--
intToStr :: Integer -> String
intToStr n = show n

strToInt :: String -> Integer
strToInt n = read n :: Integer

charToInt :: Char -> Integer
charToInt c = strToInt([c])

lastDigit1 :: Integer -> Integer
lastDigit1 n = charToInt(last(intToStr(n)))

dropLastDigit1 :: Integer -> Integer
dropLastDigit1 n = strToInt(reverse(tail(reverse(intToStr(n)))))

--
-- Exercise 1 (with math functions as suggested)
--
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

--
-- Exercise 2 (first version)
--
strToDigits :: String -> [Integer]
strToDigits [] = []
strToDigits (c:cs) = charToInt(c) : strToDigits(cs)

toDigits1 :: Integer -> [Integer]
toDigits1 n
 | n < 1     = []
 | otherwise = strToDigits(intToStr(n))

--
-- Exercise 2 (using dropLastDigit and lastDigit)
--
toDigits :: Integer -> [Integer]
toDigits n
 | n < 1     = []
 | n < 10    = [n]
 | otherwise = toDigits(dropLastDigit(n)) ++ [lastDigit(n)]

--
-- Exercise 3
--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n:ns) =
  (if length(ns) `mod` 2 == 0 then n else n*2) : doubleEveryOther(ns)

--
-- Exercise 4
--
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:ns) =
  (if n < 10 then n else sumDigits(toDigits(n))) + sumDigits(ns)

--
-- Exercise 5
--
validate :: Integer -> Bool
validate n =
  lastDigit(sumDigits(doubleEveryOther(toDigits(n)))) == 0

--
-- For checking some validations
--
e1 = 4012888888881881 -- true
e2 = 4012888888881882 -- false
e3 = 4111111111111111 -- true

--
-- Exercise 6
--
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c
 | n == 0    = []
 | otherwise = hanoi n' a c b  ++ [(a, c)] ++ hanoi n' b a c
     where n' = n - 1

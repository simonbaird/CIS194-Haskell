{-
Name: Simon Baird
Collaborators:
Notes:
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.
lastDigit :: Integer -> Integer
dropLastDigit :: Integer -> Integer

--
-- Exercise 1
--
intToStr :: Integer -> String
intToStr n = show n

strToInt :: String -> Integer
strToInt n = read n :: Integer

charToInt :: Char -> Integer
charToInt c = strToInt([c])

lastDigit n = charToInt(last(intToStr(n)))
dropLastDigit n = strToInt(reverse(tail(reverse(intToStr(n)))))


--
-- Exercise 2
--
strToDigits :: String -> [Integer]
strToDigits [] = []
strToDigits (c:cs) = charToInt(c) : strToDigits(cs)

toDigits1 :: Integer -> [Integer]
toDigits1 n
 | n < 1     = []
 | otherwise = strToDigits(intToStr(n))

-- Probably supposed to use dropLastDigit...
toDigits :: Integer -> [Integer]
toDigits n
 | n < 1     = []
 | n < 10    = [n]
 | otherwise = toDigits(dropLastDigit(n)) ++ [lastDigit(n)]

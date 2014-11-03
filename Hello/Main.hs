--
-- Some ints
--
x, y :: Int
x = 1
y = x + 2

--
-- Factorial
--
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

fac :: Int -> Int
fac n
    | n == 0    = 1
    | otherwise = n * fac(n-1)

--
-- Main
--
main = do
  putStrLn("hello")
  print(y)
  print(factorial(6))
  print(fac(6))

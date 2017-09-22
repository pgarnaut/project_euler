-- problem 2

--fibs 0 = 1
--fibs 1 = 1
--fibs n = fibs (n-1) + fibs (n-2)

import Data.List

fibz :: Integer -> Integer
fibz n = fibz' 0 1 1 where
  fibz' count x y = if count == n
    then x
    else fibz' (count + 1) y (x+y)

-- skip 1 from the list of fibonacci numbers, but still return it if fib(0) requested
fibzL :: Integer -> [Integer]
fibzL 0 = [1]
fibzL n = fibzL' 0 1 1 [] where
  fibzL' count x y l = if count == n
    then l
    else fibzL' (count+1) y (x+y) (l ++ [y])
    


fibzmax :: Integer -> Integer -> [Integer]
fibzmax 0 _ = [1]
fibzmax n m = fibzL' 0 1 1 [] where
  fibzL' count x y l = if count == n || y >= m
    then l
    else fibzL' (count+1) y (x+y) (l ++ [y])
    

main = do
  print $  foldl (+) 0 (filter even (fibzmax 9999999 4000000))
{-If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
-}


-- this is the worst damn code i've ever written :\ 

module Worderizer where
  
singles = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

-- these are unique to 11 - 19, 111-119 etc
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

splitDigits :: Int -> [Int]
splitDigits n = [n - mod n 100, (mod n 100) - (mod n 10), mod n 10]

-- if last part of number is "something-teen" e.g. 819217
isTeen n = k >= 10 && k <= 19 where
  k = (mod n 100)


secondPart n = ("and" ++ (singles !! (digits !! 2)))
  where digits = splitDigits n

makeStr :: Int -> String
makeStr 1000 = "onethousand" -- meh. ugly special case, only gonna handle up to 1000
--makeStr 10 = "ten"
makeStr n = 
  let 
    digits = splitDigits n
    -- "seventeen" or "seven" or ""
    part3 = if isTeen n then teens!!(digits!!2) else singles !! (digits!!2)
    -- "twenty"
    part2 = if (digits!!1) >= 20 then tens !! ((digits!!1) `div` 10 - 2) else ""
    -- "sixhundred"
    part1 = if (digits!!0) > 0 then (singles!!(digits!!0 `div` 100) ++ "hundred") else ""
    s1 = if (not $ null part1) && (not $ null part3) && (digits!!1 < 20) then "and" else ""
    s2 = if (not $ null part1) && (not $ null part2) && (not $ isTeen n) then "and" else ""
  in 
    part1 ++ s2 ++ part2 ++ s1 ++ part3  
    

-- length $ foldl (++) "" $ map makeStr [1..1000]
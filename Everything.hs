det a b c = b^2 - 4*a*c
quadsol1 a b c = (-b - sqrt (det a b c))/2*a
quadsol2 a b c = (-b + sqrt (det a b c))/2*a


third_a (x) = x!!2
--https://wiki.haskell.org/How_to_work_on_lists

third_b (x:y:z:xs) = z
--https://www.haskell.org/tutorial/patterns.html

fact 0 = 1
fact x = x * fact(x-1)

hailstone x = (case x `mod` 2 of
 1 -> 3*x + 1
 0 -> x `div` 2)
 
hailLen 1 = 0
hailLen x = 1 + hailLen(hailstone x)

--Primes and Divisors
divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]
primes :: Int -> [Int]
primes n = [i | i <- [2..n], divisors i == []]


--Pythagorean Triples
pythagorean :: Int -> [(Int, Int, Int)]
pythagorean x = [ (i, j, k) | i <- [1..x] , j <-[i..x], k <-[1..x], k^2 == i^2 + j^2]


--Joining Strings
join :: String -> [String] -> String
join n [] = []
join n [x] = x
join n (x:xs) = concat [x, n, (join n xs)]
--searched how to concatenate strings
--https://hoogle.haskell.org/?hoogle=concat


--Factorial with a fold
fact' x = foldl (*) 1[1,2..x]
--used stackoverflow for foldl help
--https://stackoverflow.com/questions/49246652/haskells-foldr-foldl-definitions-trip-newbie-for-foldl-actual-function-takes-f


--Tail Recursive Hailstone
hailstone x = (case x `mod` 2 of
 1 -> 3*x + 1
 0 -> x `div` 2)
 --hailstone from exer1

hailLen n = hailTail 0 n
 where
   hailTail a 1 = a
   hailTail a n = hailTail (a+1)(hailstone n)

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate


--Merging
merge :: Ord x=> [x] -> [x] -> [x]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x >= y = y : merge (x:xs) ys 
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)


--Merge Sort
split :: [x] -> ([x],[x])
split list = splitAt((length list + 1) `div` 2) list
--https://hoogle.haskell.org/?hoogle=splitAt

mergeSort :: Ord x=> [x] -> [x]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where (left,right) = split xs
-- Debugging help on how to make left and right using split: https://stackoverflow.com/questions/37082925/haskell-merge-sort


--Haskell Library and Dates
daysInYear :: Integer -> [Day]
daysInYear y = [jan1..dec31]
  where jan1 = fromGregorian y 1 1
        dec31 = fromGregorian y 12 31

isFriday :: Day -> Bool
isFriday n = snd(mondayStartWeek (n)) == 5
--https://downloads.haskell.org/~ghc/8.2.2/docs/html/libraries/time-1.8.0.2/Data-Time-Calendar-OrdinalDate.html#v%3AmondayStartWeek
--https://hoogle.haskell.org/?hoogle=snd

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

getDay :: (Integer, Int, Int) -> Int
getDay (x,y,z) = z

isPrimeDay :: Day -> Bool
isPrimeDay n = divisors(getDay(toGregorian (n))) == []

primeFridays :: Integer -> [Day]
primeFridays n = filter isPrimeDay (filter isFriday (daysInYear n))

merge :: Ord x=> [x] -> [x] -> [x]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x >= y = y : merge (x:xs) ys 
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)

--Pascal's Triangle
pascal :: Integer -> [Integer]
pascal 0 = [1]
pascal 1 = [1,1]
pascal n = map (uncurry (+)) (zip ([0] ++ pascal (n-1)) (pascal (n-1) ++ [0]))

{-
1 3 3 1
into 
1 4 6 4 1
Use:
0 1 3 3 1
1 3 3 1 0 
---------- +
1 4 6 4 1
-}


--Pointfree Addition
addPair :: Num a => (a,a) -> a
addPair = \ (x,y) -> x+y


--Pointfree Filtering
withoutZeros :: (Eq a) => (Num a) => [a] -> [a]
withoutZeros = filter (/=0) 


--Searching? Maybe?
findElt :: (Eq a) => a -> [a] -> Maybe Int
findElt n xs = go 0 n xs
    where
        go i n [] = Nothing
        go i n (x:xs)
            | n == x = Just i
            | otherwise = go (i+1) n xs

-- Reference: Needed help figuring out how to make a tracker variable.
-- Originally tried to pattern match findElt n [] = Nothing but did not work.
-- https://stackoverflow.com/questions/58073233/finding-the-index-of-an-element-in-a-list

import Data.Ratio

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

divisors :: Int -> [Int]
divisors n = [i | i <- [2..(n `div` 2)], n `mod` i == 0]

rationalSum :: Int -> [Ratio Int]
rationalSum n = go 1 n
    where
        go i 1 = []
        go i n = [i % (n-1)] ++ go (i+1) (n-1)

gcdCheck :: Int -> Int -> Bool
gcdCheck x y = gcd x y /= 1


rationalSumLowest :: Int -> [Ratio Int]
rationalSumLowest n = go 1 n
    where
        go i 1 = []
        go i n 
            | (gcd i n) == 1 = [i % (n-1)] ++ go (i+1) (n-1)
            | otherwise = go (i+1) (n-1)

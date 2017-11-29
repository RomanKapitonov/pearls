module LeastAbsentNumber where

import Data.Array
import Data.Array.ST
import Data.List hiding (
      (\\))

-- Naïve solution
-----------------------------------------------------------------------------------
minfree1 :: [Int] -> Int
minfree1 xs = head([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
xs \\ ys = filter (flip (notElem) ys) xs

-- Array based solution
-----------------------------------------------------------------------------------

minfree2 :: [Int] -> Int
minfree2 = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
     where n = length xs

-----------------------------------------------------------------------------------

minfree3 :: [Int] -> Int
minfree3 = search' . countlist

search' :: Array Int Int -> Int
search' = length . takeWhile (/= 0) . elems

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip (filter (<= n) xs) (repeat 1))
     where n = length xs

-- Using state monad
-----------------------------------------------------------------------------------

minfree4 :: [Int] -> Int
minfree4 = search . checklist'

checklist' xs = runSTArray $ do
  let n = length xs
  a <- newArray (0, n) False
  sequence [writeArray a x True | x <- xs, x <= n]
  return a

-- Using divide-and-conquer
-----------------------------------------------------------------------------------

minfree5 :: [Int] -> Int
minfree5 = minfrom1 0

minfrom1 :: Int -> [Int] -> Int
minfrom1 a xs = head ([a..] \\ xs)

-----------------------------------------------------------------------------------

minfree6 :: [Int] -> Int
minfree6 xs = minfrom2 0 (length xs, xs)

minfrom2 :: Int -> (Int, [Int]) -> Int
minfrom2 a (n, xs)
  | n == 0         = a
  | m == b - a     = minfrom2 b (n - m, vs)
  | otherwise      = minfrom2 a (m, us)
    where (us, vs) = partition (<b) xs
          b        = a + 1 + n `div` 2
          m        = length us

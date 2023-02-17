import Data.List
import Primes

--abundant = filter (\x -> (divSum x) > x) $ ([2,4..] `union` [945..])
--abu n = takeWhile (<n) abundant
abuHelp 1 l = l
abuHelp x l
--  | x < 945 && x `mod` 2 /= 0 = abuHelp (x-1) l
  | divSum x > x = abuHelp (x-1) (x:l)
  | otherwise = abuHelp (x-1) l
abu x = abuHelp x []
abuAll = abu 20161
abuAllR = reverse abuAll

isAbuSumHelp x lo up
  | lo + up > x = isAbuSumHelp x lo (up-1) 
  | lo + up < x = isAbuSumHelp x (lo+1) up
  | divSum lo > lo && divSum up > up = True
  | lo >= up = False
  | otherwise = isAbuSumHelp x lo (up-1)
isAbuSum x = isAbuSumHelp x 1 (x-1)
isAbuSumHelp' x lo up
  | lo >= length abuAll = False
  | up >= length abuAllR = False
  | (abuAll!!lo) + (abuAllR!!up) > x = isAbuSumHelp' x lo (up+1) 
  | (abuAll!!lo) + (abuAllR!!up) < x = isAbuSumHelp' x (lo+1) up
  | (abuAll!!lo) + (abuAllR!!up) == x = True
  | (abuAll!!lo) >= (abuAllR!!up) = False
  | otherwise = isAbuSumHelp' x lo (up+1)
isAbuSum' x = isAbuSumHelp' x 0 0

--abuSums x = nub . filter (<x) $ concat [map (+n) (abu x) | n <- (abu x)]

primAbuHelper cur l 
  | any (cur `divBy`) l = primAbuHelper (cur+1) l
  | (divSum cur) > cur = cur : primAbuHelper (cur+1) (cur:l)
  | otherwise = primAbuHelper (cur+1) l

primAbu = primAbuHelper 1 []

abuSumsHelp :: [Int] -> [Int]
abuSumsHelp [] = []
abuSumsHelp (x:xs) = nub $ ((zipWith (+) abuAll (x:xs)) `union` (abuSumsHelp xs))
abuSums = abuSumsHelp abuAll


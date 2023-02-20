module Primes
( primes, divBy, primesOf, isPrime, divSum )
where

import qualified Data.IntMap.Strict as IM
import Data.List

primes :: [Int]
primes = 2 : filter (\n -> all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)) [3,5..]

divBy a b                                                                 
  | a `mod` b == 0 = True
  | otherwise = False      

primesOf :: Int -> [Int]
primesOf n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

isPrime :: Int -> Bool
isPrime n = f n primes
  where
    f n (p:ps)
      | p*p > n = True
      | n `mod` p == 0 = False
      | otherwise = f n ps

divSum :: Int -> Int 
divSum n = (f n (nub (primesOf n)) 1) - n
  where f n [] r = 1
        f n (x:xs) r
          | n `divBy` r = f n (x:xs) (r*x)  
          | otherwise   = ( (r-1) `div` (x-1) ) * (f n xs (head xs))

getDivisors :: Int -> [Int]
getDivisors n = filter (\x->n`divBy`x) [2..(n`div`2)]


-- Sieve of Eratosthenes
zeros :: Int -> IM.IntMap Int
zeros 2 = IM.singleton 2 0
zeros n = IM.insert n 0 (zeros (n-1))

primeMapR :: Int -> Int -> IM.IntMap Int
primeMapR n limit
  | n == 2           = sieve n (n+n) (zeros limit) limit
  | prev IM.! n == 0 = sieve n (n+n) prev limit
  | otherwise        = prev
  where prev = primeMapR (n-1) limit
        sieve n c curMap sz
          | c > sz = curMap
          | otherwise = IM.adjust (+1) c (sieve n (c+n) curMap sz)

primeMap :: Int -> IM.IntMap Int
primeMap sz = primeMapR sz sz

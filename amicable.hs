import Primes

divBy a b 
    | a `mod` b == 0 = True 
    | otherwise = False

primesOf :: Int -> [Int]
primesOf n =
    let ps = take n primes 
    in filter (divBy n) ps

divSum :: Int -> [Int] -> Int -> Int
divSum n [] r = 1
divSum n (x:xs) r
    | n `divBy` r = divSum n (x:xs) (r*x)
    | otherwise = ( (r-1) `div` (x-1) ) * divSum n xs (head xs)

divSum' n = divSum n (primesOf n) 1 - n

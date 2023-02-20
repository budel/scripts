import Primes
import Data.List

ring (x:xs) d = (x+d*4):(x+d*3):(x+d*2):(x+d):x:xs

--spiral 2 [1]
--spiral max d (x:xs) 
--  | d>=max = xs
--  | otherwise = spiral max (d+2) ((x+d*4):(x+d*3):(x+d*2):(x+d):x:xs)

--7*7-6*4:7*7-6*3:7*7-6*2:7*7-6:7*7:[]
spiral 1 xs = xs
spiral n xs = spiral (n-2) (n*n-(n-1)*3:n*n-(n-1)*2:n*n-(n-1):n*n:xs)

--spiralPrimeRatio =
--p58 n = (genericLength . filter (\x->head (primesOf x)==x) $ spiral n []) / (genericLength (spiral n [])+1)
p58 n = (genericLength . filter isPrime $ spiral n []) / (toRational ((n-1)*2+1))
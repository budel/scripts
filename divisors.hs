import Primes

triNum :: Integral a => a -> a
triNum 1 = 1
triNum x = x + triNum (x-1)

numDivisors :: Integral a => a -> a -> Int -> Int
numDivisors x 1 l = 2*l
numDivisors x y l
              | x == y = numDivisors x (floor(sqrt(fromIntegral y))) (l+1)
              | x `mod` y /= 0 = numDivisors x (y-1) l
              | otherwise = numDivisors x (y-1) (l+1)
--triNumLen = [(x, length [1 | y <- x:[x`div`2,(x`div`2-1)..1], x `mod` y == 0]) | x <- triNum]

getDivisors :: Int -> [Int]
getDivisors n = filter (\x->n`divBy`x) [2..(n`div`2)]

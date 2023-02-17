fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

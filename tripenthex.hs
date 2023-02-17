
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

tri :: Int -> Int
tri = (map triM [0..] !!)
  where triM 1 = 1
        triM n = (tri(n-1))+n
tris = map tri [1..]
isTri n = triQ == fromInteger(round(triQ))
  where triQ = (sqrt(8*n+1)-1)/2

pen :: Int -> Int
pen = (map penM [0..] !!)
  where penM 1 = 1
        penM n = (pen(n-1))+(3*(n-1)+1)
pens = map pen [1..]
isPen n = pentQ == fromInteger(round(pentQ))
  where pentQ = (1+sqrt(24*n+1))/6 

hex :: Int -> Int
hex = (map hexM [0..] !!)
  where hexM 1 = 1
        hexM n = (hex(n-1))+(4*n-3)
hexs = map hex [1..]

getMultiples y x = if x<y then [] else [y]
getMultiplesOf3 n = [3..n]
sumMultiplesOf3 :: Integer -> Integer
sumMultiplesOf3 0 = 0
sumMultiplesOf3 n = (if mod n 3 == 0 then n else 0) + sumMultiplesOf3  (n-1)
sumMultiplesOf5 :: Integer -> Integer
sumMultiplesOf5 0 = 0
sumMultiplesOf5 n = (if mod n 5 == 0 then n else 0) + sumMultiplesOf5  (n-1)
sumMulti3or5 :: Integer -> Integer
sumMulti3or5 0 = 0
sumMulti3or5 n = (if mod n 3 == 0 || mod n 5 == 0
		 then n
		 else 0) + sumMulti3or5 (n-1)


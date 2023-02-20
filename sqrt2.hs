
sq2 :: Int -> Rational
sq2 i
    | i==0 = 0
    | otherwise = toRational (1/(2+sq2 (i-1)))

sqrt2 :: Int -> Rational
sqrt2 i = 1 + sq2 i

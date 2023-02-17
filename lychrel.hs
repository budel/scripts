

revInt n = read (reverse . show $ n) :: Integer
isPalin n = n == (revInt n)

isLychrelH :: Integer -> Int -> Bool
isLychrelH n it 
  | isPalin n = False
  | it > 50 = True
  | otherwise = isLychrelH (n + revInt n) (it+1)

isLychrel :: Integer -> Bool
isLychrel n = isLychrelH (n + revInt n) 1


lenCycleH c f n  
  | c > 1000 = (n,0)
  | fst (divList c f n) == snd (divList c f n) = (n,c)
  | otherwise = lenCycleH (c+1) (f*100) n 
  where divList c f n = splitAt c . show $ f `div` n
lenCycle n 
  | n < 10 = lenCycleH 1 100 n
  | n < 100 = lenCycleH 1 1000 n
  | n < 1000 = lenCycleH 1 10000 n
  | n < 10000 = lenCycleH 1 100000 n
  | otherwise = error("Number too high")

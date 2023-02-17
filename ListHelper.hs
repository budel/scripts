--module ListHelper (
--list2Int,
--pair2List
--) where

toIntHelp :: [Integer] -> Integer -> Integer
toIntHelp [] _ = 0
toIntHelp (x:xs) f = (f*x) + toIntHelp xs (f*10)

list2Int :: [Integer] -> Integer
list2Int l = toIntHelp (reverse l) 1

pair2List :: (a, a) -> [a]
pair2List (x,y) = [x,y]
 
nextC xs = tail . take ((length xs)+1) . cycle $ xs
cyclesOfHelp :: Eq a => [a] -> [a] -> [[a]]
cyclesOfHelp xs o | xs == o = [xs]
cyclesOfHelp xs o = xs : cyclesOfHelp (nextC xs) o
cyclesOf :: Eq a => [a] -> [[a]]
cyclesOf xs = cyclesOfHelp (nextC xs) xs

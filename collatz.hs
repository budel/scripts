import Data.Ord
import Data.List

collatz :: Integral a => a -> [a]
collatz x
        | x == 1 = [1]
        | x `mod` 2 == 0 = x:collatz (x `div` 2)
        | otherwise = x:collatz (3*x+1)
collatzUniq :: Integral a => a -> [a] -> [[a]]
collatzUniq x forbidden
            | x == 1 = []
            | x `elem` forbidden = collatzUniq (x-1) forbidden
            | otherwise = collatz x:collatzUniq (x-1) (nub (forbidden ++ collatz x))
argmax f l = maximumBy (comparing f) l

collatzMaxLen :: Integral a => a -> Int -> a -> (a, Int)
--collatzMaxLen 1 l a = (a,l)
collatzMaxLen 888889 l a = (a,l)
collatzMaxLen x l a
                  | length (collatz x) < l = collatzMaxLen (x-1) l a
                  | otherwise = collatzMaxLen (x-1) (length (collatz x)) x

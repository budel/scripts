module Main where
import Data.Mod
import Data.List (nub)

type Mod5 = Mod 50515093

sns :: Int -> Mod5
sns = (map f [0..] !!)
  where f 0 = 290797 :: Mod5
        f n = (f (n-1) * f (n-1)) :: Mod5

p :: Int -> (Mod5, Mod5)
p n = (sns (2*n), sns (2*n+1))

dist :: Num a => (a, a) -> (a, a) -> a
dist (p1,p2) (q1,q2) = (q1-p1)*(q1-p1) + (q2-p2)*(q2-p2)

distOf2 :: Num a => [(a, a)] -> a
distOf2 xs = dist (head xs) (xs!!1)

combinationsWRep :: Eq a => [a] -> Int -> [[a]]
combinationsWRep xs n = filter ((n==).length.nub) $ mapM (const xs) [1..n]

main :: IO ()
main = do
    let ps = map p [0..14]
    print $ minimum (map distOf2 $ combinationsWRep ps 2)
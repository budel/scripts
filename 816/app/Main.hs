module Main where
import Data.Mod
import qualified Data.Set as DS

type Mod5 = Mod 50515093

s0Inv = 290707^%(-1)::Mod5

sns :: Int -> Mod5
sns = (map f [0..] !!)
  where f 0 = 290797 :: Mod5
        f n = (f (n-1) * f (n-1)) :: Mod5

p :: Int -> (Mod5, Mod5)
p n = (sns (2*n), sns (2*n+1))

dist :: Num a => (a, a) -> (a, a) -> a
dist (p1,p2) (q1,q2) = abs (p1-q1) + abs (p2-q2)
euclDist :: Num a => (a, a) -> (a, a) -> a
euclDist (p1,p2) (q1,q2) = (q1-p1)*(q1-p1) + (q2-p2)*(q2-p2)

distOf2 :: Num a => DS.Set (a, a) -> a
distOf2 xs = dist (DS.elemAt 0 xs) (DS.elemAt 1 xs)

main :: IO ()
main = do
    --let ps = map p [0..2000000]
    let ps = map p [0..13]
    let psSet = DS.fromList ps
    let combs = DS.filter (\x->length x==2) . DS.powerSet $ psSet
    print $ minimum . map distOf2 $ DS.toList combs
    print s0Inv

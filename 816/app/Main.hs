module Main where
import Data.Mod.Word
import qualified Data.Set as DS

type Mod5 = Mod 50515093

s0Inv :: Maybe Mod5
s0Inv = invertMod (290707 :: Mod5)

sns :: Int -> Mod5
sns = (map f [0..] !!)
  where f :: Int -> Mod5
        f 0 = 290797 :: Mod5
        f n = (f (n-1) * f (n-1)) :: Mod5

p :: Int -> (Mod5, Mod5)
p n = (sns (2*n), sns (2*n+1))

euclDist :: (Integral a, Floating b) => (a, a) -> (a, a) -> b
euclDist (p1,p2) (q1,q2) = sqrt.fromIntegral $ (q1-p1)*(q1-p1) + (q2-p2)*(q2-p2)
euclDistMod :: Floating a => (Mod5, Mod5) -> (Mod5, Mod5) -> a
euclDistMod (p1,p2) (q1,q2) = euclDist (unMod p1, unMod p2) (unMod q1, unMod q2) 
euclDistOf2 :: Floating a => DS.Set (Mod5, Mod5) -> a
euclDistOf2 xs = euclDistMod (DS.elemAt 0 xs) (DS.elemAt 1 xs)

dist :: Num a => (a, a) -> (a, a) -> a
dist (p1,p2) (q1,q2) = abs (p1-q1) + abs (p2-q2)
distOf2 :: Num a => DS.Set (a, a) -> a
distOf2 xs = dist (DS.elemAt 0 xs) (DS.elemAt 1 xs)

main :: IO ()
main = do
    --let ps = map p [0..2000000]
    --print $ length . DS.fromList $ map sns [0..20000]
    let ps = map p [0..13]
    let psSet = DS.fromList ps
    let combs = DS.filter (\x->length x==2) . DS.powerSet $ psSet
    let min1 = minimum . map euclDistOf2 $ DS.toList combs
    print (min1::Double)
    let best = DS.elemAt 0 $ DS.filter (\s->euclDistOf2 s == min1) combs
    print best
    print s0Inv

module Main where
import Data.Mod

type Mod4 = Mod 4503599627370517

ec :: Mod4 -> Mod4
ec n = ((1504170715041707*n) :: Mod4)

ecsHelp :: Mod4 -> Mod4 -> Mod4 -> [Mod4]
ecsHelp l c i
  | c < l = c:ecsHelp c (ec i) (i+1)
  | otherwise = ecsHelp l (ec i) (i+1)

ecs :: [Mod4]
ecs = (ec 1) : ecsHelp (ec 1) (ec 2) 2

modInv :: Mod4
modInv = 1504170715041707 ^% (-1) :: Mod4

ecInv :: Mod4 -> Mod4
ecInv candidate = (modInv*candidate) :: Mod4

ecsIHelp :: Mod4 -> Mod4 -> Mod4 -> [Mod4]
ecsIHelp m n candidate
  | n < m = candidate : ecsIHelp n (ecInv (candidate+1)) (candidate+1)
  | otherwise = ecsIHelp m (ecInv (candidate+1)) (candidate+1)

ecsInv :: [Mod4]
ecsInv = 1 : ecsIHelp (ecInv 1) (ecInv 2) 2

main :: IO ()
main = do
  putStrLn "Start"
  let firstFew = take 16 ecs
  let beginning = map unMod (firstFew)
  print beginning
  print (modInv)
  let lastFew = takeWhile (<last firstFew) $ ecsInv
  let coins = map unMod (firstFew++(reverse lastFew))
  print (coins)
  print (sum coins)

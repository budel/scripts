module Main where

import qualified Data.HashMap.Strict as HM
import Algorithm.Search (aStarAssoc)
import Data.Maybe (fromMaybe)
import Data.List (unfoldr)

graph2 :: HM.HashMap String [(String, Int)]
graph2 = HM.fromList
  [ ("0", [("1", 3)])
  , ("1", [("2", 7), ("3", 4)])
  , ("2", [("4", 2), ("5", 4)])
  , ("3", [("5", 4), ("6", 6)])
  , ("4", [("7", 8), ("8", 5)])
  , ("5", [("8", 5), ("9", 9)])
  , ("6", [("9", 9), ("10", 3)])
  ]
--   3
--  7 4
-- 2 4 6
--8 5 9 3

invDists :: HM.HashMap k [(a, Int)] -> HM.HashMap k [(a, Int)]
invDists = HM.map (map (\(n,d)->(n,d*(-1))))

sumTo :: [a] -> Int
sumTo xs = sum [1..(length xs-1)]
parentNodes :: [a] -> [String]
parentNodes xs = map show [sumTo xs - (length xs-2) .. sumTo xs]
compNodes :: [a] -> [String]
compNodes xs = map show . take (length xs) $ [sumTo xs+1..]

dec2bin :: Int -> [Int]
dec2bin = unfoldr (\x -> if x==0 then Nothing else Just(rem x 2, div x 2))

main :: IO ()
main = do
  print "Reading tree.txt"
  content <- readFile "tree.txt"
  let linesOfFile = lines content
  let myTreeList = take 4 $ map (map read . words) linesOfFile :: [[Int]]
  print myTreeList

  let nodeDist = map (\xs -> zipWith (curry (:[])) (compNodes xs) xs) myTreeList
  let parents = map parentNodes myTreeList
  let left = zipWith zip parents nodeDist
  let right = zipWith (\ p d -> zip p (tail d)) parents nodeDist
  let g1 = HM.unions (zipWith (\ l r -> HM.unionWith (++) (HM.fromList l) (HM.fromList r)) left right)
  let treeGraph = HM.insert "0" (head (head nodeDist)) g1
  print treeGraph

  let costFunction node = fromMaybe [] (HM.lookup node (invDists treeGraph))
  let r _ = 0
  let shortest = minimum $ map (\end -> aStarAssoc costFunction r (== end) "0") (compNodes (last myTreeList))
  print shortest
  let bestWay = "0":snd (fromMaybe (0, ["error"]) shortest)
  let myWay = zipWith (\step nodes -> if step == fst (head nodes) then head nodes else nodes!!1) (tail bestWay) (map costFunction bestWay)
  print myWay

  let depth = length myTreeList-1
  let perms_ = map dec2bin [0..(2^depth-1)]
  let perms = map (\xs-> xs ++ replicate (depth-(length xs)) 0 ) perms_
  let cstFn node = fromMaybe [] (HM.lookup node treeGraph)
  let start = (cstFn "0")!!0
  let ways = map (scanl (\x y -> (cstFn (fst x)!!y) ) start) perms
  let sums = map (sum . map (\(_,d)->d)) ways
  let best = filter (\w -> (sum (map (\(_,d)->d) w)) == (maximum sums)) ways
  print $ maximum sums
  print best

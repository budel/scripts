module Main where

import qualified Data.HashMap.Strict as HM
import Algorithm.Search (dijkstraAssoc)
import Data.Maybe (fromMaybe)

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

main :: IO ()
main = do
  print "Reading tree.txt"
  content <- readFile "tree.txt"
  let linesOfFile = lines content
  let myTreeList = map (map read . words) linesOfFile :: [[Int]]
  print myTreeList

  let nodeDist = map (\xs -> zipWith (curry (:[])) (compNodes xs) xs) myTreeList
  let parents = map parentNodes myTreeList
  let left = zipWith zip parents nodeDist
  let right = zipWith (\ p d -> zip p (tail d)) parents nodeDist
  let g1 = HM.unions (zipWith (\ l r -> HM.unionWith (++) (HM.fromList l) (HM.fromList r)) left right)
  let treeGraph = HM.insert "0" (head (head nodeDist)) g1
  print treeGraph

  let costFunction node = fromMaybe [] (HM.lookup node (invDists treeGraph))
  let shortest = minimum $ map (\end -> dijkstraAssoc costFunction (== end) "0") (compNodes (last myTreeList))
  print shortest
  let bestWay = "0":snd (fromMaybe (0, ["error"]) shortest)
  let myWay = zipWith (\step nodes -> if step == fst (head nodes) then head nodes else nodes!!1) (tail bestWay) (map costFunction bestWay)
  print myWay

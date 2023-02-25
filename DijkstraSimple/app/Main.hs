module Main where

import qualified Data.HashMap.Strict as HM
import DijkstraSimple

invDists :: Graph -> Graph
invDists graph = Graph $ (HM.map (map (\(n,d)->(n,d*(-1)))) $ (edges graph))

graph1 :: Graph
graph1 = Graph $ HM.fromList
  [ ("A", [("D", 100), ("B", 1), ("C", 20)])
  , ("B", [("D", 50)])
  , ("C", [("D", 20)])
  , ("D", [])
  ]

graph2 :: Graph
graph2 = Graph $ HM.fromList
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

sumTo :: [a] -> Int
sumTo xs = sum [1..((length xs)-1)]
parentNodes :: [a] -> [String]
parentNodes xs = map show [sumTo xs - ((length xs)-2) .. sumTo xs]
compNodes :: [a] -> [String]
compNodes xs = map show . take (length xs) $ [(sumTo xs)+1..]

main :: IO ()
main = do
  print "Reading tree.txt"
  content <- readFile "tree.txt"
  let linesOfFile = lines content
  let myTreeList = map (map read) (map words linesOfFile) :: [[Int]]
  print (myTreeList)

  let nodeDist = map (\xs -> map (:[]) $ zip (compNodes xs) xs) myTreeList
  let parents = map parentNodes myTreeList
  let left = map (\(p,d)->zip p d) $ zip parents nodeDist
  let right = map (\(p,d)->zip p (tail d)) $ zip parents nodeDist
  let g1 = HM.unions . map (\(l,r) -> HM.unionWith (++) (HM.fromList l) (HM.fromList r)) $ zip left right
  let treeGraph = Graph $ HM.insert "0" (nodeDist!!0!!0) g1
  print treeGraph

  let graph2Inv = invDists treeGraph
  let shortest = map (findShortestDistance graph2Inv "0") (compNodes (last myTreeList))
  print shortest
  print (minimum shortest)

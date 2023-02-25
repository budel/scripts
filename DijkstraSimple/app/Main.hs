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
  , ("4", [("7", 8), ("8", 2)])
  , ("5", [("8", 5), ("9", 9)])
  , ("6", [("9", 9), ("10", 3)])
  ]
--   3
--  7 4
-- 2 4 6
--8 5 9 3

addEdge:: String -> [(String,Int)] -> HM.HashMap String [(String,Int)] -> HM.HashMap String [(String,Int)]
addEdge node edge = HM.insertWith (++) node edge

sumTo :: [a] -> Int
sumTo xs = sum [1..((length xs)-1)]
parentNodes :: [a] -> [String]
parentNodes xs = map show [sumTo xs - ((length xs)-2) .. sumTo xs]
compNodes :: [a] -> [String]
compNodes xs = map show . take (length xs) $ [(sumTo xs)+1..]

main :: IO ()
main = do
  print "Example"
  let shortest = findShortestDistance graph1 "A" "D"
  print shortest
  let graph2Inv = invDists graph2
  let shortest2 = map (findShortestDistance graph2Inv "0") ["7","8","9","10"]
  print shortest2
  print (minimum shortest2)

  content <- readFile "tree.txt"
  let linesOfFile = lines content
  let treeHead = (read . head $ linesOfFile) :: Int
  let g = HM.singleton "0" [("1", treeHead)]
  let g1 = addEdge "1" [("3", 4)] . addEdge "1" [("2", 7)] $ g
  --HM.insertWith (++) "2" [("4", 2)] g
  --print (drop 1 linesOfFile)
  let myTreeList = map words linesOfFile
  print (myTreeList)
  print (map parentNodes myTreeList)
  print (map compNodes myTreeList)
  print g1

import Data.List

data Score = HighCard | OnePair | TwoPairs | ThreePairs | Straight | Flush | FullHouse | StraightFlush | RoyalFlush deriving (Eq, Ord, Show, Read, Bounded, Enum)  

--order after sorting: [2..9] A J K Q T
score :: [String] -> Score
score (a:b:c:d:e)
  | a!!0=='A' && b!!0=='J' && c!!0=='K' && d!!0=='Q' && e!!0=='T' = RoyalFlush

main = do
   content <- readFile "poker.txt"
   let linesOfFile = lines content
   let hands = map (splitAt 15) linesOfFile
   let p1scores = map sort . map words . map fst $ hands
   print p1scores


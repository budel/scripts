import Data.List

data Suit = C | D | H | S deriving (Eq, Ord, Show, Read, Bounded, Enum)
type Rank = Int
data Card = Card Rank Suit deriving (Eq, Ord, Show, Read, Bounded)

toRank :: String -> Rank
toRank text = 
  case text of 
    "T" -> 10
    "J" -> 11
    "Q" -> 12
    "K" -> 13
    "A" -> 14
    d -> read d :: Int

toCard :: String -> Card
toCard text = Card (toRank (text!!0:"")) (read (text!!1:"") :: Suit)


data Score = HighCard | OnePair | TwoPairs | Triple | Straight | Flush | FullHouse | SameFour | StraightFlush | RoyalFlush deriving (Eq, Ord, Show, Read, Bounded, Enum)  

rank (Card r s) = r
suit (Card r s) = s

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

lenUniq = length . nub

isStraight xs = all (==1) $ zipWith (-) (tail xs) xs
isFlush xs = (length . nub $ xs) == 1
isStraightFlush cards = isStraight (map rank cards) && isFlush (map suit cards)
hasFourSame xs = allTheSame (init xs) || allTheSame (tail xs)
hasTriple xs = allTheSame (take 3 xs) || allTheSame (take 3 (tail xs)) || allTheSame (take 3 (tail (tail xs)))
hasPair (a:b:c:d:e:[]) = (a==b) || (b==c) || (c==d) || (d==e)


score :: [Card] -> Score
score (x:xs)
  | rank x==10 && isStraightFlush (x:xs) = RoyalFlush
  | isStraightFlush (x:xs) = StraightFlush
  | hasFourSame (map rank (x:xs)) = SameFour
  | lenUniq (map rank (x:xs)) == 2 && hasPair (map rank (x:xs)) = FullHouse
  | isFlush (map suit (x:xs)) = Flush
  | isStraight (map rank (x:xs)) = Straight
  | hasTriple (map rank (x:xs)) = Triple
  | lenUniq (map rank (x:xs)) == 3 = TwoPairs
  | hasPair (map rank (x:xs)) = OnePair
  | otherwise = HighCard

getDiffHighCard (c1,c2) = head . dropWhile (\(a,b)->a==b) $ zip (reverse (map rank c1)) (reverse (map rank c2)) 
hasP1HigherCard xs = fst (getDiffHighCard xs) > snd (getDiffHighCard xs)

pairRank hand = (nub (map rank hand)) \\ (map rank hand)
hasP1HigherPair (h1, h2) = pairRank h1 > pairRank h2
  
hasP1Won :: ([Card], [Card]) -> Bool
hasP1Won (c1, c2) 
  | score c1 > score c2 = True
  | score c1 < score c2 = False
  | score c1 == HighCard = hasP1HigherCard (c1, c2)
  | score c1 == OnePair = hasP1HigherPair (c1, c2)

main = do
   content <- readFile "poker.txt"
   let linesOfFile = lines content
   let handsStr = map words $ linesOfFile
   let handsStrSorted = map (\(a,b)-> (sort a,sort b)) . map (splitAt 5) $ handsStr
   let hands = map (splitAt 5) . map (map toCard) $ handsStr
   let handsSorted = map (\(h1, h2) -> (sortBy (\a b->compare (rank a) (rank b)) h1, sortBy (\a b->compare (rank a) (rank b)) h2)) hands
   let p1Won = map hasP1Won handsSorted
   --print (take 5 $ zip handsStrSorted handsSorted)
   print (take 5 $ zip3 handsStrSorted (map (score . fst) handsSorted) (map (score . snd) handsSorted))
   print (length . filter hasP1Won $ handsSorted)




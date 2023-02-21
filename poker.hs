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
hasPair (a:b:c:d:e:_) = a==b || b==c || c==d || d==e


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
  | hasPair (x:xs) = OnePair
  | otherwise = HighCard
  

main = do
   content <- readFile "poker.txt"
   let linesOfFile = lines content
   let handsStr = map words $ linesOfFile
   let hands = map (splitAt 5) . map (map toCard) $ handsStr
   let p1scores = map score . map sort . map fst $ hands
   let p2scores = map score . map sort . map snd $ hands
   print (zip3 handsStr p1scores p2scores)



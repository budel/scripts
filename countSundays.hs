import Data.Time 
import Data.Time.Calendar

firstDay = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "1901-1-1"
firstDay = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2000-12-31"

getLst (_,_,a) = a
isFirstOfMonth n | getLst (toGregorian n) == 1 = True | otherwise = False

length $ filter (==Sunday) (map dayOfWeek (filter isFirstOfMonth ([firstDay..lastDay] :: [Day])))

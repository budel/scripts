numWord :: Integral a => a -> String
numWord n
        | n < 20 = case n of {
	0 -> "";
        1 -> "one";
        2 -> "two";
        3 -> "three";
        4 -> "four";
	5 -> "five";
	6 -> "six";
	7 -> "seven";
	8 -> "eight";
	9 -> "nine";
	10 -> "ten";
	11 -> "eleven";
	12 -> "twelve";
	13 -> "thirteen";
	14 -> "fourteen";
	15 -> "fifteen";
	16 -> "sixteen";
	17 -> "seventeen";
	18 -> "eighteen";
	19 -> "nineteen";
	}
	| n < 100 = case (n `div` 10) of {
        2 -> "twenty" ++ (numWord (n `mod` 10));
	3 -> "thirty" ++ (numWord (n `mod` 10));
	4 -> "forty" ++ (numWord (n `mod` 10));
	5 -> "fifty" ++ (numWord (n `mod` 10));
	6 -> "sixty" ++ (numWord (n `mod` 10));
	7 -> "seventy" ++ (numWord (n `mod` 10));
	8 -> "eighty" ++ (numWord (n `mod` 10));
	9 -> "ninety" ++ (numWord (n `mod` 10));
	}
	| n < 1000 = case (n `div` 100) of {
	1 -> "onehundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	2 -> "twohundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	3 -> "threehundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	4 -> "fourhundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	5 -> "fivehundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	6 -> "sixhundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	7 -> "sevenhundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	8 -> "eighthundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	9 -> "ninehundred" ++ (if n `mod` 100 /= 0 then "and" else "") ++ (numWord (n `mod` 100));
	}
	| otherwise = error "not implemented yet"


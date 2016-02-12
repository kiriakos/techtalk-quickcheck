import Test.QuickCheck
import Data.List (intersperse)

-- Concatenates a list of strings into one String joining it 
-- with a character
unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]

powers :: Number -> [Number]
powers n = n : map (* n) powers n

-- show
split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs'' = dropWhile (/=c) xs

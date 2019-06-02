import Data.Char

onlyTwo :: Int -> Bool
onlyTwo x | x == 2 = True
          | otherwise = False

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = x == last xs && isPalindrome xs

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx idx array = last part
                         where part = take (idx+1) array 

capitalize :: [Char] -> [Char]
capitalize (x:xs) = (toUpper x) : xs

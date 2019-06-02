import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc list = (reverse . sort) list

sortDesc' :: Ord a => [a] -> [a]
sortDesc' = reverse . sort


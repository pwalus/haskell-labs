isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc list = (==0) . length . filter (\(x,y) -> y <= x) . zip list $ tail list 




zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' a b c = zipWith (\x (y,z) -> (x,y,z) ) a $ zip b c





concat' :: [[a]] -> [a]
concat' list = foldr (\x acc -> x ++ acc) [] list

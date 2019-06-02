sumWith :: Num a => (a -> b) -> [a] -> a
sumWith f = foldr (\x acc -> f x + acc) 0

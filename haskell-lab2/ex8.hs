max' :: (Ord a) => [a] -> a
max' [] = error "Maximum of empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)


replicate' :: (Num a, Ord a) => a -> b -> [b] 
replicate' a b | a <= 0 = []
               | otherwise =  b : (replicate' (a - 1) b)


take' :: (Num a, Ord a) => a -> [b] -> [b]
take' a [] = []
take' a (x:xs) | a <= 0 = []
               | otherwise = x : take' (a - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq b) => b -> [b] -> Bool
elem' _ [] = False 
elem' search (x:xs) | x == search = True
                    | otherwise = elem' search xs


quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = let leftPart = quickSort (filter (<x) xs)
                       rightPart = quickSort (filter (>=x) xs)
                   in leftPart ++ [x] ++ rightPart

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | (f x) == True = x : filter' f xs
                 | otherwise = filter' f xs

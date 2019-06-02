import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc = (reverse . sort) 


--are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
--are2FunsEqAt _ _ [] = True
--are2FunsEqAt f g (x:xs) = (&&) . (f x == g x) are2FunsEqAt f g xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x == True = x : filter' f xs
                 | otherwise = filter' f xs


onlyEven = filter' even

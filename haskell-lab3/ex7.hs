map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 

map'' :: (a -> b) -> [a] -> [b]
map'' f tab = foldr (\x acc -> f x : acc) [] tab


map''' :: (a -> b) -> [a] -> [b]
map''' f tab = foldl (\acc x -> acc ++ [f x] ) [] tab



filter' :: (a -> Bool) -> [a] -> [a]
filter' f list = foldr (\x acc -> if f x == True then x : acc else acc) [] list

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f list = foldl (\acc x -> if f x == True then acc ++ [x] else acc ) [] list


sumwith' :: Num a => (a->a) -> [a] -> a
sumwith' _ [] = 0
sumwith' f (x:xs) = f x + sumwith' f xs



prodWith' :: Num a => (a->a) -> [a] -> a
prodWith' _ [] = 1
prodWith' f (x:xs) = f x * prodWith' f xs


sumwith'' :: Num a => (a->a) -> [a] -> a
sumwith'' f tab = let loop acc _ [] = acc
                      loop acc f (x:xs) = loop (acc + f x) f xs
                     in loop 0 f tab



foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr f acc xs)



foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

:w


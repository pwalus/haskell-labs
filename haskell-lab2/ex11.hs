prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs


length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs


and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = y == x || elem' y xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) | even x = x : selectEven xs
                  | otherwise = selectEven xs

--avg' :: Fractional a => [a] -> a
--avg' [] = 0
--avg' (x:xs) = (x + avg' xs) / length xs





sum2 :: Num a => [a] -> a
sum2 xs = loop 0 xs
          where loop acc [] = acc
                loop acc (x:xs) = loop (x + acc) xs 

prod2 :: Num a => [a] -> a
prod2 tab = loop 1 tab
            where loop acc [] = acc
                  loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 tab = loop 0 tab
               where loop acc [] = acc
                     loop acc (x:xs) = loop (acc + 1) xs



elem3' :: Eq a => a -> [a] -> Bool
elem3' y (x:xs) | y == x && length xs == 1 = True 
                | y /= x && length xs /= 1 = False
                | otherwise = elem3' y xs
            
              


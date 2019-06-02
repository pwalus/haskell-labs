sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs


prod'2 :: Num a => [a] -> a
prod'2 x = loop 1 x
       where loop acc [] = acc
             loop acc (x:xs) = loop (acc * x) xs

length'2 :: [a] -> Int
length'2 x = loop 1 x
         where loop acc [] = 0
               loop acc (x:xs) = acc + loop acc xs

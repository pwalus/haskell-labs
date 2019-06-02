sides :: [(Int,Int,Int)]
sides = [(a,b,c) | a <- [1..100], b <- [1..100], c<- [1..100], a^2 + b^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

genPrime :: Int -> [Int]
genPrime n = sito [1..n]
             where sito :: [Int] -> [Int]
                   sito [x:xs] =

genPrime n = filter isDivided [x | x <- [1..n]]
             where isDivided y = y 

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' _ [] = 0
sumWith' f (x:xs) = f x + sumWith' f xs


sum = sumWith' (\x -> x)
sumSqr = sumWith' (\x -> x^2)
sumCube = sumWith' (\x -> x^3)
sumAbs = sumWith'(\x -> abs x)

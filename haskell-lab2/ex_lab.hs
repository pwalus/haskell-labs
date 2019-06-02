allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = length [y | y <- xs, x == y] > 1


fib :: (Num a, Eq a) => a -> a
fib n | n == 0 || n == 1 = n
      | otherwise = fib(n - 1) + fib(n - 2) 


prod' :: Num a => [a] -> a
prod' tab = loop 1 tab
            where loop acc [] = acc
                  loop acc (x:xs) =  loop (x * acc) xs

length' :: Num a => [a] -> a
length' tab = loop 0 tab
              where loop acc [] = acc
                    loop acc (x:xs) = loop (acc + 1) xs

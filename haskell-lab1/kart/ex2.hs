sgn :: Int -> Int
sgn x = if x > 0
        then 1
        else if x == 0
             then 0
        else -1

sgn' :: Int -> Int
sgn' x | x > 0 = 1
       | x == 0 = 0
       | otherwise  = -1

isDigit :: Int -> Bool
isDigit x = if x > 0 && x < 10
            then True
            else False

isDigitC :: Int -> Bool
isDigitC x | x > 0 && x < 10 = True
           | otherwise = False


isDigitn :: Int -> Bool
isDigitn 1 = True
isDigitn 2 = True
isDigitn 3 = True
isDigitn 4 = True
isDigitn 5 = True
isDigitn _ = False 


isDigtn :: Int -> Bool
isDigtn x = case x of
            1 -> True
            2 -> True
            3 -> False


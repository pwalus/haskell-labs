not' :: Bool -> Bool
not' b = case b of
         True -> False
         False -> True

absInt' :: Int -> Int
absInt' x = case (x >= 0) of
            True -> x
            _ -> -x

isAnswer :: String -> Bool
isAnswer x = case (x == "Love") of
             True -> True
             _ -> False 

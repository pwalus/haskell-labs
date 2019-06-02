absInt :: Int -> Int
-- absInt x = if x < 0 then x * (-1) else x
absInt x | x < 0 = -x
         | otherwise = x

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x <= y then x else y 

min3Int :: (Int, Int, Int) -> Int
--min3Int (x,y,z) = if x <= y && x <= z
--                  then x
--                  else if y <= x && y <= z
--                       then y
--                      else z

min3Int (x,y,z) | x <= y && x <= z = x
                | y <= x && y <= z = y
                | otherwise = z


min3IntEx :: (Int, Int, Int) -> Int
min3IntEx (x,y,z) = min2Int(x, min2Int(y, z)) 


sgn :: Int -> Int
sgn x | x > 0 = 1
      | x == 0 = 0
      | x < 0 = -1


isDigit :: Char -> Bool
isDigit x | x == '1' = True
          | x == '2' = True
          | otherwise = False

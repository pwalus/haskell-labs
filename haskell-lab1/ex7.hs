--not' :: Bool -> Bool
--not' True = False
--not' False = True

isTheAnswer :: Int -> Bool
isTheAnswer 1 = False
isTheAnswer _ = True 


isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True -- :)
isItTheAnswer _      = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' _ = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' _ = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True) = False
nand' _ = True

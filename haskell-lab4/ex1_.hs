newtype Cos = Cos String

newtype Lol a = Lol a

lol' :: Floating a => Lol a -> a
lol' (Lol x) = x + 2

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y


add2C :: Num a => a -> ( a -> a )
add2C x y = x + y

add1Plus_ = add2C 1
add1Plus2 = add1Plus_ 2


add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> a -> a -> a
add3C x y z = x + y + z


fiveToPower_ :: Integer -> Integer
fiveToPower_ x = (5 ^ x)

_ToPower5 :: Num a => a -> a
_ToPower5 x = (x^5)


--flip2 :: (a -> b -> c) -> b -> a -> ci
flip2 f = g
          where g a b = f b a

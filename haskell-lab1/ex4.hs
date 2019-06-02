sqr :: Double -> Double
sqr x = x * x

vec2Dlen :: (Double, Double) -> Double
vec2Dlen (x, y) = sqrt(x^2 + y^2)

vec3Dlen :: (Double, Double, Double) -> Double
vec3Dlen (x, y, z) = sqrt(x^2 + y^2 + z^2) 

swap :: (Int, Char) -> (Char, Int)
swap (x, y) = (y , x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && y  == z

heron :: (Double, Double, Double) -> Double
heron (a,b,c) = sqrt(p * (p - a) * (p - b) * (p - c))
                where p = (a + b + c) / 2

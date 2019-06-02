--roots :: (Double, Double, Double) -> (Double, Double)
--roots (a,b,c) = ((-b - d) / e, (-b + d) / e)
--                where d = sqrt(b^2 - 4 * a * c)
--                      e = 2 * a

roots' :: (Double, Double, Double) -> (Double, Double)
roots' (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

rootsOne :: (Double, Double, Double) -> (Double, Double)
rootsOne (a,b,c) = let e = 2 * a
                       d = sqrt(b^2 - 4*a*c)
                   in ( (-b - d) / e, (-b + d) / e )

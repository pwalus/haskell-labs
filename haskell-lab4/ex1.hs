polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r, pi) = (r * cos pi, r * sin pi) 

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)


newtype CartesianCoord'' a = MkCartesianCoord'' (a,a) deriving (Show)
newtype PolarCoord'' a = MkPolarCoord'' (a,a) deriving (Show)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r, phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString :: PersonInfo' -> String
personInfoToString (nm, snm, addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr


newtype PersonInfo'' = PersonInfo'' ([Char], [Char], [Char]) 

personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (PersonInfo''(nm, snm, addr)) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr


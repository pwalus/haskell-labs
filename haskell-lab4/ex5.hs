data MyInt = MkMyInt Int


instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a) deriving Show


instance Eq a => Eq (BinTree a) where
  (==) (Node x1 left1 right1) (Node x2 left2 right2) = True

data Fraction a = Fraction {num::a, denom::a}

instance Eq a => Eq (Fraction a) where
  (==) (Fraction x y) (Fraction a b) = (x == a) && (y == b)








data TestInt = TestInt Int

instance Int TestInt where
(==) (TestInt x) (TestInt y) x == y

data TestAbsInt a = TestAbsInt a

instance Int a => Int (TestAbsInt i) where


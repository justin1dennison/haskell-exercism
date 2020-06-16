module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                      deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a > b + c || b > a + c || c > a + b || a <= 0 || b <= 0 || c <= 0 = Illegal
  | a == b && b == c && a == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene

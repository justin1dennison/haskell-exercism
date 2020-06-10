module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show)

valueOf :: Color -> Int
valueOf Black = 0
valueOf Brown = 1
valueOf Red = 2
valueOf Orange = 3
valueOf Yellow = 4
valueOf Green = 5 
valueOf Blue = 6
valueOf Violet = 7
valueOf Grey = 8
valueOf White = 9


value :: (Color, Color) -> Int
value (a, b) = 10 * valueOf a + valueOf b

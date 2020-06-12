module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color = Black
           | Brown
           | Red
           | Orange
           | Yellow
           | Green
           | Blue
           | Violet
           | Grey
           | White
               deriving (Show, Enum, Bounded)

newtype Resistor = Resistor{bands :: (Color, Color, Color)}
                     deriving Show

encode :: Color -> Int
encode Black = 0
encode Brown = 1
encode Red = 2
encode Orange = 3
encode Yellow = 4
encode Green = 5
encode Blue = 6
encode Violet = 7
encode Grey = 8
encode White = 9

resistanceWithUnit :: Int -> (String, String)
resistanceWithUnit n
  | n < thousand = (show n, "ohms")
  | n >= thousand && n < million = (show (div n thousand), "kiloohms")
  | n >= million && n < billion = (show (div n million), "megaohms")
  | otherwise = (show (div n billion), "gigaohms")
  where thousand = 10 ^ (3 :: Integer)
        million = 10 ^ (6 :: Integer)
        billion = 10 ^ (9 :: Integer)

label :: Resistor -> String
label resistor = value ++ " " ++ unit
  where (value, unit) = resistanceWithUnit $ ohms resistor

ohms :: Resistor -> Int
ohms resistor = (10 * xi + yi) * 10 ^ zi
  where (x, y, z) = bands resistor
        xi = encode x
        yi = encode y
        zi = encode z


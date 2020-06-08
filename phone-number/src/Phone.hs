module Phone (number) where

import Data.Char (isDigit)

validate :: String -> Maybe String
validate xs
  | len == 11 = if firstNumber == '1' then validate $ tail xs else Nothing
  | len == 10 =
    if firstNumber `elem` ['0', '1'] || fourthNumber `elem` ['0', '1'] then Nothing else
      Just xs
  | otherwise = Nothing

  where len = length xs
        firstNumber = head xs
        fourthNumber = head $ drop 3 xs

clean :: String -> Maybe String
clean xs = Just $ filter isDigit xs

number :: String -> Maybe String
number input = clean input >>= validate


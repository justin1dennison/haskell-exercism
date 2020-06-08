module Phone
  ( number
  ) where

import Data.Char (isDigit)

validate :: String -> Maybe String
validate ('0':_) = Nothing
validate ('1':_) = Nothing
validate (_:_:_:'0':_) = Nothing
validate (_:_:_:'1':_) = Nothing
validate xs = Just xs

clean :: String -> Maybe String
clean xs
  | len == 11 =
    if head filtered == '1'
      then Just $ tail filtered
      else Nothing
  | len == 10 = Just filtered
  | otherwise = Nothing
  where
    filtered = filter isDigit xs
    len = length filtered

number :: String -> Maybe String
number input = clean input >>= validate

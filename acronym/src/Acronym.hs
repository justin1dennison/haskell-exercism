module Acronym
  ( abbreviate
  ) where

import Data.Char

capitalize :: String -> String
capitalize [] = []
capitalize [x] = [toUpper x]
capitalize (x:xs) = toUpper x : xs

capitalLettersOnly :: String -> String
capitalLettersOnly = filter isAsciiUpper

firstLetterOfAllCapitalWord :: String -> String
firstLetterOfAllCapitalWord xs =
  if all isAsciiUpper xs
    then take 1 xs
    else xs

removePunctuation :: Char -> Char
removePunctuation x =
  if isAlpha x
    then x
    else ' '

abbreviate :: String -> String
abbreviate xs =
  concatMap (capitalLettersOnly . firstLetterOfAllCapitalWord . capitalize) $
  words $ map removePunctuation $ filter (/= '\'') xs

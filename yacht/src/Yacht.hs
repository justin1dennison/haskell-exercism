module Yacht (yacht, Category(..)) where

import Data.List (group, sort, sortOn)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

basic :: Int -> [Int] -> Int
basic n = (* n) . length . filter (== n)

isFullHouse :: [Int] -> Bool
isFullHouse = (== [2, 3]) . sort . map length . group . sort

isFourOfAKind :: [Int] -> Bool
isFourOfAKind = (>= 4) . length . last . sortOn length . group . sort

isLittleStraight :: [Int] -> Bool
isLittleStraight = (== [1, 2, 3, 4, 5]) . sort

isBigStraight :: [Int] -> Bool
isBigStraight = (== [2, 3, 4, 5, 6]) . sort

isYacht :: [Int] -> Bool
isYacht = (== 1) . length . group

yacht :: Category -> [Int] -> Int
yacht Ones dice = basic 1 dice
yacht Twos dice = basic 2 dice
yacht Threes dice = basic 3 dice
yacht Fours dice = basic 4 dice
yacht Fives dice = basic 5 dice
yacht Sixes dice = basic 6 dice
yacht FullHouse dice = if isFullHouse dice then sum dice else 0
yacht FourOfAKind dice
  = if isFourOfAKind dice then
      sum $ take 4 $ last $ sortOn length $ group $ sort dice else 0
yacht LittleStraight dice = if isLittleStraight dice then 30 else 0
yacht BigStraight dice = if isBigStraight dice then 30 else 0
yacht Choice dice = sum dice
yacht Yacht dice = if isYacht dice then 50 else 0

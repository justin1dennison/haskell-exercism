module TwelveDays (recite) where

import Data.List (intercalate)

recite :: Int -> Int -> [String]
recite start stop = map recite' [start .. stop]

recite' :: Int -> String
recite' 1 = refrain 1 ++ gift 1 ++ "."
recite' n = refrain n ++ gifts
  where gs = map gift [1 .. n]
        f = intercalate ", " . reverse . tail
        gifts = concat [f gs, ", and ", head gs, "."]

gift :: Int -> String
gift 12 = "twelve Drummers Drumming"
gift 11 = "eleven Pipers Piping"
gift 10 = "ten Lords-a-Leaping"
gift 9 = "nine Ladies Dancing"
gift 8 = "eight Maids-a-Milking"
gift 7 = "seven Swans-a-Swimming"
gift 6 = "six Geese-a-Laying"
gift 5 = "five Gold Rings"
gift 4 = "four Calling Birds"
gift 3 = "three French Hens"
gift 2 = "two Turtle Doves"
gift 1 = "a Partridge in a Pear Tree"
gift _ = error "Invalid day"

refrain :: Int -> String
refrain n
  = "On the " ++ ordinal ++ " day of Christmas my true love gave to me: "
  where ordinal
          = case n of
                1 -> "first"
                2 -> "second"
                3 -> "third"
                4 -> "fourth"
                5 -> "fifth"
                6 -> "sixth"
                7 -> "seventh"
                8 -> "eighth"
                9 -> "ninth"
                10 -> "tenth"
                11 -> "eleventh"
                12 -> "twelfth"
                _ -> error "Invalid number"

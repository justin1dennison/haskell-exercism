module ETL (transform) where
import Data.Char (toLower)

import Data.Map (Map, fromList, toList)

lowercase :: String -> String
lowercase = map toLower

transform :: Map a String -> Map Char a
transform = fromList . concatMap f . toList
  where f (score, letters) = zip (lowercase letters) $ repeat score

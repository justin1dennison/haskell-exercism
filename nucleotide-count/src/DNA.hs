module DNA (nucleotideCounts, Nucleotide(..)) where
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A
                | C
                | G
                | T deriving (Eq, Ord, Show)

encode :: Char -> Either String Nucleotide
encode 'A' = Right A
encode 'C' = Right C
encode 'G' = Right G
encode 'T' = Right T
encode x = Left $ show x

count :: [Nucleotide] -> Map Nucleotide Int
count = M.fromList . map pair . L.group . L.sort
  where pair g = (head g, length g)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = count <$> mapM encode xs


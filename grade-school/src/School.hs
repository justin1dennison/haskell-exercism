module School (School, add, empty, grade, sorted) where
import qualified Data.List as L

import qualified Data.Map as M

type Grade = Int

type Student = String

data School = School{roster :: M.Map Grade [Student]}

                deriving (Show)

add :: Grade -> Student -> School -> School
add gradeNum student school
  = School{roster = M.insertWith (++) gradeNum [student] $ roster school}

empty :: School
empty = School{roster = M.fromList []}

grade :: Int -> School -> [Student]
grade gradeNum school
  = case M.lookup gradeNum $ roster school of
        Just students -> L.sort students
        Nothing -> []

sorted :: School -> [(Grade, [Student])]
sorted school = M.toAscList $ M.map L.sort $ roster school

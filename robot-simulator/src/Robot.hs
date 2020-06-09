{-# LANGUAGE NamedFieldPuns #-}
module Robot
       (Bearing(East, North, South, West), bearing, coordinates, mkRobot, move)
       where

data Bearing = North
             | East
             | South
             | West
                 deriving (Eq, Show)

type Point = (Integer, Integer)

data Robot = Robot{direction :: Bearing, position :: Point}

bearing :: Robot -> Bearing
bearing = direction

coordinates :: Robot -> Point
coordinates = position

mkRobot :: Bearing -> Point -> Robot
mkRobot dir coords = Robot{direction = dir, position = coords}

move :: Robot -> String -> Robot
move = foldl move'

move' :: Robot -> Char -> Robot
move' Robot{direction, position} 'R' = mkRobot (turnRight direction) position
move' Robot{direction, position} 'L' = mkRobot (turnLeft direction) position
move' Robot{direction, position} 'A'
  = mkRobot direction (advance direction position)
move' _ _ = error "Invalid move"

advance :: Bearing -> Point -> Point
advance North (x, y) = (x, y + 1)
advance East (x, y) = (x + 1, y)
advance West (x, y) = (x - 1, y)
advance South (x, y) = (x, y - 1)

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

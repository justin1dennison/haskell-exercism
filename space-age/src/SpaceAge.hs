module SpaceAge
  ( Planet(..)
  , ageOn
  ) where

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

secondsToEarthYear :: Float -> Float
secondsToEarthYear seconds = seconds / 31557600

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = (secondsToEarthYear seconds) / 0.2408467
ageOn Venus seconds = (secondsToEarthYear seconds) / 0.61519726
ageOn Earth seconds = (secondsToEarthYear seconds) / 1.0
ageOn Mars seconds = (secondsToEarthYear seconds) / 1.8808158
ageOn Jupiter seconds = (secondsToEarthYear seconds) / 11.862615
ageOn Saturn seconds = (secondsToEarthYear seconds) / 29.447498
ageOn Uranus seconds = (secondsToEarthYear seconds) / 84.016846
ageOn Neptune seconds = (secondsToEarthYear seconds) / 164.79132

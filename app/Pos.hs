module Pos where

data Pos = Pos {line :: Int, column :: Int} deriving (Eq, Ord)
instance Show Pos where
  show pos = show (line pos) ++ ":" ++ show (column pos)

incrementLine :: Pos -> Pos
incrementLine pos = pos {line = (line pos) + 1}
incrementColumn :: Pos -> Pos
incrementColumn pos = pos {column = (column pos) + 1}




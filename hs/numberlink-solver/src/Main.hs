module Main where

import qualified Data.Map as M

problem :: [String]
problem = 
  [ "...2"
  , ".1.."
  , "..2."
  , "1..."
  ]

-- data Point = Point Int Int deriving (Eq, Show)
type Pos = (Int, Int)
data Cell = Cell {
    cState :: CellState
  , cType  :: CellType
} deriving Show

data CellState = Empty | Number Int deriving Show

data CellType = Fixed | NonFixed deriving Show

type Field = M.Map Pos Cell

mkEmptyCell :: Cell
mkEmptyCell = Cell Empty NonFixed

mkFixedCell :: Int -> Cell
mkFixedCell n = Cell (Number n) Fixed

mkField :: [String] -> Field
mkField input = M.fromList $ map convert $ zip [0..] (concat input)
  where
    convert :: (Int, Char) -> (Pos, Cell)
    convert (id, c) 
      | c == '.'  = (pos, mkEmptyCell)
      | otherwise = (pos, mkFixedCell $ read [c])
      where
        pos = (id `div` m, id `mod` n)

    n = length $ head input
    m = length input

main :: IO ()
main = do
    print $ mkField problem

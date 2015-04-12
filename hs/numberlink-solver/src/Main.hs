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
} deriving (Eq,Show)

data CellState = Empty | Number Int deriving (Eq,Show)

data CellType = Start | Goal | Normal deriving (Eq,Show)

type Field = M.Map Pos Cell

mkEmptyCell :: Cell
mkEmptyCell = Cell Empty Normal

mkStartCell :: Int -> Cell
mkStartCell n = Cell (Number n) Start

mkGoalCell :: Int -> Cell
mkGoalCell n = Cell (Number n) Goal

mkField :: [String] -> Field
mkField input = convert (zip [0..] $ concat input) M.empty
  where
    convert :: [(Int, Char)] -> Field -> Field
    convert [] field = field
    convert ((id, c):ts) field 
      | c == '.'  = convert ts $ M.insert pos mkEmptyCell field
      | otherwise = convert ts $ if hasValue value 
                                   then M.insert pos (mkStartCell value) field
                                   else M.insert pos (mkGoalCell  value) field
      where
        pos = (id `div` m, id `mod` n)
        value = read [c] :: Int
        hasValue n = mkStartCell n `elem` M.elems field

    n = length $ head input
    m = length input

main :: IO ()
main = do
    print $ mkField problem

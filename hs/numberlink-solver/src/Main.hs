module Main where

import Data.List (maximumBy)
import Data.Function (on)
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

showField :: Field -> String
showField field = chunk $ concatMap (show . getNumber . cState) $ M.elems field
  where (_,m) = fieldSize field
        chunk [] = []
        chunk ss = let (a, b) = splitAt m ss
                   in  a ++ "\n" ++ chunk b
 
getNumber :: CellState -> Int
getNumber Empty = 0
getNumber (Number n) = n

mkEmptyCell :: Cell
mkEmptyCell = Cell Empty Normal

mkCell :: Int -> Cell
mkCell n = Cell (Number n) Normal

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
                                   then M.insert pos (mkGoalCell value) field
                                   else M.insert pos (mkStartCell value) field
      where
        pos = (id `div` m, id `mod` n)
        value = read [c] :: Int
        hasValue n = mkStartCell n `elem` M.elems field

    n = length $ head input
    m = length input

main :: IO ()
main = do
    let field = mkField problem
        answer = head $ solve $ findStart 1 field
    print answer

solve :: (Pos, Field) -> [(Pos, Field)]
solve (pos, field) 
  | isCompleted field = return ((-1,-1), field)
  | otherwise = [(p,fld) | (p, fld) <- next pos field] >>= solve

isCompleted :: Field -> Bool
isCompleted field = all (not . isEmpty field) $ M.keys field

next :: Pos -> Field -> [(Pos, Field)]
next p field = case M.lookup p field of
                 Just (Cell (Number n) Goal) -> nextStart n field
                 _ -> map update (validNextPos p field)
  where
    value = getValue p field
    update pos = (pos, M.update (\_ -> Just $ mkCell value) pos field)

nextStart :: Int -> Field -> [(Pos, Field)]
nextStart n field = if n == maxNum
                      then [((-1,-1),field)]
                      else [findStart (n+1) field]
  where
    maxNum = maximum $ map (getNumber . cState) $ M.elems field


findStart :: Int -> Field -> (Pos, Field)
findStart n field = let pos = head $ M.keys $ M.filter (isStartN n) field
                    in  (pos, field)
  where
    isStartN n (Cell cellState cellType) = cellState == Number n && cellType == Start

getValue :: Pos -> Field -> Int
getValue p field = case M.lookup p field of
                     Nothing -> 0
                     Just (Cell Empty _) -> 0
                     Just (Cell (Number n) _) -> n

fieldSize :: Field -> Pos
fieldSize field = let (n,m) = last $ M.keys field
                  in (n+1,m+1)

inField :: Field -> Pos -> Bool
inField field (x,y) = 0 <= x && x < n && 0 <= y && y < m
  where (n,m) = fieldSize field

isEmpty :: Field -> Pos -> Bool
isEmpty field p = case M.lookup p field of
                    Just (Cell Empty _) -> True
                    _ -> False

isStart :: Field -> Pos -> Bool
isStart field p = case M.lookup p field of
                    Just (Cell _ Start) -> True
                    _ -> False

isGoalN :: Int -> Field -> Pos -> Bool
isGoalN n field p = case M.lookup p field of
                    Just (Cell (Number m) Goal) -> n == m
                    _ -> False

nextPos :: Pos -> [Pos]
nextPos (x,y) = [(x+1,y),(x,y-1),(x-1,y),(x,y+1)]

validNextPos :: Pos -> Field -> [Pos]
validNextPos p field = filter (\pos -> isEmpty field pos || not (isStart field pos)) $ filter (inField field) $ nextPos p

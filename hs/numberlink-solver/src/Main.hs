module Main where

import System.Environment (getArgs)
import qualified Data.Map as M

type Pos = (Int, Int)
data Cell = Cell {
    cState :: CellState
  , cType  :: CellType
} deriving (Eq,Show)

data CellState = Empty | Number Int deriving (Eq,Show)

data CellType = Start | Goal | Normal deriving (Eq,Show)

type Field = M.Map Pos Cell

type CurrentState = (Int, Pos, Field)

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
    args <- getArgs
    case args of
      [] -> print "pass filename of problem"
      [filename] -> do
        p <- readFile filename
        let field = mkField $ lines p
            (_,_,f) = head $ solve $ findStart 1 field
        putStr $ showField f

solve :: CurrentState -> [CurrentState]
solve (n, pos, field) 
  | isCompleted field = return (-1, (-1,-1), field)
  | n == -1 = []
  | otherwise = [ nextState 
                | nextState@(_,_,nextField) <- next n pos field
                , not $ isVerbose nextState
                , not $ hasOrphan nextField pos 
                ] >>= solve
                            
isVerbose :: CurrentState -> Bool
isVerbose (n, p, field) = 2 <= length sameNumberCells
  where sameNumberCells = filter (\(Just (Cell s _)) -> getNumber s == n)
                        $ map (`M.lookup` field) 
                        $ filter (not . isGoalN n field) 
                        $ filter (inField field) 
                        $ nextPos p

hasOrphan :: Field -> Pos -> Bool
hasOrphan field p = any ((<= 1) . availableCount field)
                        $ filter (isEmpty field) 
                        $ filter (inField field) 
                        $ nextPos p

availableCount :: Field -> Pos ->Int
availableCount field p = length
                       $ filter (not . isUsed field) 
                       $ filter (inField field) 
                       $ nextPos p

isCompleted :: Field -> Bool
isCompleted field = all (not . isEmpty field) $ M.keys field

next :: Int -> Pos -> Field -> [CurrentState]
next n p field = case M.lookup p field of
                 Just (Cell (Number _) Goal) -> nextStart n field
                 _ -> map update (validNextPos n p field)
  where
    update pos = (n, pos, M.update func pos field)
    func (Cell Empty _) = Just $ mkCell n
    func (Cell (Number m) Goal) = Just $ mkGoalCell m

nextStart :: Int -> Field -> [CurrentState]
nextStart n field = if n == maxNum
                      then [(-1, (-1,-1),field)]
                      else [findStart (n+1) field]
  where
    maxNum = maximum $ map (getNumber . cState) $ M.elems field

findStart :: Int -> Field -> CurrentState
findStart n field = let pos = head $ M.keys $ M.filter (isStartN n) field
                    in  (n, pos, field)
  where
    isStartN n (Cell cellState cellType) = cellState == Number n && cellType == Start

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

isUsed :: Field -> Pos -> Bool
isUsed field p = case M.lookup p field of
                    Just (Cell (Number _) Normal) -> True
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

validNextPos :: Int -> Pos -> Field -> [Pos]
validNextPos n p field = filter (\pos -> isEmpty field pos || isGoalN n field pos) $ filter (inField field) $ nextPos p

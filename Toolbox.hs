module Toolbox where

import Control.Applicative
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )

class Display a where
  display :: a -> String

instance Display a => Display [a] where 
  display [] = ""
  display (x:xs) = display x ++ (display xs)

data Square = Square (Maybe Integer) Loc deriving Show

instance Eq Square where
  s == s' = loc s == loc s'

instance Display Square where 
  display (Square (Just v) _) = (show v)
  display (Square Nothing _) = [boxChar]

data Problem = SquareUnfillable Square | NumberUnmatchable Region Integer | SquareWithMultipleSolutions Square [Integer]

data Loc = Loc (Integer, Integer) deriving (Eq, Show)
r (Loc (a, _)) = a
c (Loc (_, b)) = b

instance Ord Loc where 
  (Loc (row, col)) `compare` (Loc (row', col')) = 
    case row `compare` row' of
      EQ -> col `compare` col'
      neq -> neq

data Region = Region Shape Integer deriving (Show, Eq)

data Board = Board SquaresMap Regions deriving Show

instance Display Board where 
  display b = let board = map (\row -> " " ++ (concat $ displayRow $ map display row) ++ " ") (to2dArray $ allSquares b)
                  sqs = splitRows board
              in concat $ intersperse "\n" sqs

displayRow :: [String] -> [String]                 
displayRow row = intersperse " " (intercalate ["|"] (chunk 3 row))

splitRows :: [String] -> [String]
splitRows arr = intercalate [replicate 24 '-'] (chunk 3 arr)                 

to2dArray :: [Square] -> [[Square]]
to2dArray ls = groupBy (\s s' -> (r $ loc s) == (r $ loc s')) ls

data Shape = Row | Col | Box deriving (Show, Bounded, Enum, Ord, Eq)

type Squares = [Square]
type SquaresMap = Map Loc Square
type Regions = [Region]
  
row (Square _ (Loc (r, _))) = Region Row r
col (Square _ (Loc (_, c))) = Region Col c
box (Square _ (Loc (r, c))) = Region Box ((r `quot` 3) * 3 + (c `quot` 3) + 1)

loc :: Square -> Loc
loc (Square _ l) = l

filled :: Square -> Bool
filled (Square Nothing  _)  = False
filled (Square (Just _) _)  = True

unfilled = not . filled

val :: Square ->  Integer
val (Square v loc) = fromJust v

shapes = [Row, Col, Box]
funcs = [row, col, box]
shapeFuncs = zip shapes funcs
getShapeFunc s = fromJust $ lookup s shapeFuncs

shape (Region s _) = s
num (Region _ n) = n

allSquares (Board s _) = Map.elems s

squaresMap (Board s _) = s
allRegions (Board _ r) = r

rows b = filter ((== Row) . shape) (allRegions b)
cols b = filter ((== Col) . shape) (allRegions b)
boxes b = filter ((== Box) . shape) (allRegions b)

regions :: Square -> Regions
regions = (<**> [row, col, box]) . pure
  
boxChar = '\x2B1C';
nums = [1..9]

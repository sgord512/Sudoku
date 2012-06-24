module Su.Base where

import Control.Applicative
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Util.List ( chunk )
import Util.String
import System.Random

data Solver = Solver { boardS :: Board, 
                       genS :: StdGen, 
                       problemsS :: Problems, 
                       movesS :: Moves,
                       deadEndsS :: DeadEnds
                       
                     }

data DeadEnd = DeadEnd Square Integer Problems 

instance Show DeadEnd where
  show (DeadEnd s i ps) = "Square at " ++ (show $ loc s) ++ " cannot be filled with " ++ (show i) ++ " because " ++ (concat $ intersperse ", and " (map show ps))
 
type DeadEnds = [DeadEnd]

data Move = SolutionApplication Solution | RandomMove Square [Integer]

instance Show Move where
  show (SolutionApplication soln) = "Applied solution: " ++ show soln
  show (RandomMove sq poss) = "Randomly filled in square " ++ (show sq)
              
squareInMove :: Move -> Square                                           
squareInMove (SolutionApplication (Solution sq _)) = sq
squareInMove (RandomMove sq _) = sq                                                    
                                           
instance Display Solver where
  display Solver{ movesS = moves, boardS = board } = displayColor (if null moves then Nothing else Just $ squareInMove $ head moves) board
  
displayColor lastMove b = let board = map (\row -> " " ++ (concat $ displayRow $ map (displayWithHighlighting lastMove) row) ++ " ") (to2dArray $ allSquares b)
                              sqs = splitRows board
                          in "\n" ++ (concat $ intersperse "\n" sqs) ++ "\n"
                          
type Moves = [Move]
type Problems = [Problem]              

data Solution = Solution Square Reason deriving Eq

instance Show Solution where
  show (Solution sq reason) = "Square at " ++ show (loc sq) ++ " can be filled in with " ++ show (val sq) ++ " because " ++ show reason

data Reason = OnlyAllowableValue | OnlyLocationForNumberInRegion Region deriving Eq

instance Show Reason where
  show OnlyAllowableValue = "all other values appeared already in its row, box, and column"
  show (OnlyLocationForNumberInRegion region) = "this number cannot be placed in any other square in " ++ show region

initialState seed = Solver { boardS = newBoard, 
                             genS = mkStdGen seed, 
                             problemsS = [], 
                             movesS = [], 
                             deadEndsS = [] }

class Display a where
  display :: a -> String

instance Display a => Display [a] where 
  display [] = ""
  display (x:xs) = display x ++ (display xs)

data Square = Square Loc (Maybe Integer)

instance Eq Square where
  s == s' = loc s == loc s'

displayWithHighlighting :: Maybe Square -> Square -> String
displayWithHighlighting Nothing sq = display sq
displayWithHighlighting (Just sqMove) sq | sqMove == sq = color Green (display sq)
                                         | otherwise = display sq

instance Display Square where 
  display (Square _ (Just v)) = (show v)
  display (Square _ Nothing) = [boxChar]
  
instance Show Square where
  show (Square l (Just v)) = "Square at " ++ (show l) ++ " filled in with " ++ (show v)
  show (Square l Nothing) = "Square at " ++ (show l) ++ " currently empty"

data Problem = SquareUnfillable Square | NumberUnmatchable Region Integer | SquareWithConflictingSolutions Square [Integer]

instance Display Problem where 
  display p = "Problem: " ++ show p

instance Show Problem where
  show (SquareUnfillable s) = "Square at " ++ (show $ loc s) ++ " cannot be filled!"
  show (NumberUnmatchable r i) = "There is no place to put a " ++ (show i) ++ " in " ++ (display r) ++ "!"
  show (SquareWithConflictingSolutions s ls) = "Square at " ++ (show $ loc s) ++ " must be all of the following: " ++ (show ls)
data Loc = Loc (Integer, Integer) deriving Eq
r (Loc (a, _)) = a
c (Loc (_, b)) = b

instance Show Loc where
  show (Loc (r, c)) = "(" ++ (show r) ++ "," ++ (show c) ++ ")"

instance Ord Loc where 
  (Loc (row, col)) `compare` (Loc (row', col')) = 
    case row `compare` row' of
      EQ -> col `compare` col'
      neq -> neq
      
instance Display Loc where
  display (Loc l) = show l

data Region = Region Shape Integer deriving Eq

instance Show Region where
  show = display

instance Display Region where
  display (Region s n) = show s ++ " " ++ show n

data Board = Board SquaresMap Regions deriving Show

newBoard :: Board
newBoard = let squares = [Square (Loc (row, col)) Nothing | row <- nums, col <- nums ]
               regions = nums <**> (map (\s n -> Region s n) (map fst shapeFuncs))
           in Board (Map.fromList $ zip (map loc squares) squares) regions
              


instance Display Board where 
  display b = let board = map (\row -> " " ++ (concat $ displayRow $ map display row) ++ " ") (to2dArray $ allSquares b)
                  sqs = splitRows board
              in (concat $ intersperse "\n" sqs) ++ "\n"

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
  
row (Square (Loc (r, _)) _) = Region Row r
col (Square (Loc (_, c)) _) = Region Col c
box (Square (Loc (r, c)) _) = Region Box (((r - 1) `quot` 3) * 3 + ((c - 1) `quot` 3) + 1) 

loc :: Square -> Loc
loc (Square l _) = l

filled :: Square -> Bool
filled (Square _  Nothing)  = False
filled (Square _ (Just _))  = True

unfilled = not . filled

val :: Square ->  Integer
val (Square loc v) = fromJust v

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

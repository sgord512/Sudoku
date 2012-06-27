module Su.Types where

import Control.Applicative
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Util.List ( chunk )
import Util.String
import System.Random

--| List of possible numbers that can go in each box. 
nums = [1..9]

--| Type Synonyms 
type DeadEndsMap = Map Loc [DeadEnd]
type LocValMap = Map Loc Val
type LVPair = (Loc, Val)

type Moves = [Move]
type Problems = [Problem]              
type Reasons = [Reason]
type Regions = [Region]
type Solutions = [Solution]

--| Board 
data Board = Board LocValMap Regions deriving Show

boardLocValMap :: Board -> LocValMap
boardLocValMap (Board lvMap _) = lvMap 

boardRegions :: Board -> Regions
boardRegions (Board _ r) = r

val :: Board -> Loc -> Val
val b l = Map.findWithDefault (error ("Tried to get value of invalid location: " ++ show l))  boardLocValMap l

filled :: Board -> Loc -> Bool
filled b l = case val b l of
  Empty -> False
  Num _ -> True

unfilled = not . filled

boardRows b = filter ((== Row) . regionShape) (allRegions b)
boardCols b = filter ((== Col) . regionShape) (allRegions b)
boardBoxes b = filter ((== Box) . regionShape) (allRegions b)

shapes = [Row, Col, Box]
funcs = [locRow, locCol, locBox]
shapeFuncs = zip shapes funcs

blankBoard :: Board
blankBoard = let locs = [Loc (row, col) | row <- nums, col <- nums ]
               regions = nums <**> map (\s n -> Region (fst s) n) [Row, Col, Box]
           in Board (Map.fromList $ zip locs (repeat Empty)) regions

--| DeadEnd
data DeadEnd = DeadEnd Loc Val Problems 

--| Loc
data Loc = Loc (Integer, Integer) deriving Eq

locRow (Loc (r, _)) = Region Row r
locCol (Loc (_, c)) = Region Col c
locBox (Loc (r, c)) = Region Box (((r - 1) `quot` 3) * 3 + ((c - 1) `quot` 3) + 1) 

locRegions :: Loc -> Regions
locRegions = (<**> [row, col, box]) . pure

instance Ord Loc where 
  (Loc (row, col)) `compare` (Loc (row', col')) = 
    case row `compare` row' of
      EQ -> col `compare` col'
      neq -> neq

--| Move
data Move = SolutionApplication Solution | RandomMove Loc [Val]

moveLoc :: Move -> Loc                                           
moveLoc (SolutionApplication (Solution l _)) = l
moveLoc (RandomMove l _) = l

--| Problem
data Problem = LocUnfillable Loc | NumberUnmatchable Region Integer | LocWithConflictingSolutions Loc [Integer]

--| Reason
data Reason = OnlyAllowableValue | OnlyLocForNumberInRegion Region deriving Eq

--| Region
data Region = Region Shape Integer deriving Eq

regionShape (Region s _) = s
regionNum (Region _ n) = n

--| Shape
data Shape = Row | Col | Box deriving (Show, Bounded, Enum, Ord, Eq)

--| Solution
data Solution = Solution Loc Value Reasons

solutionReasons (Solution _ _ rs) = rs
solutionLoc (Solution l _ _) = l
solutionVal (Solution _ v _) = v

instance Eq Solution where 
         (Solution l v _) == (Solution l' v' _) = (l == l') && (v == v')

--| Solver
data Solver = Solver { boardS :: Board, 
                       genS :: StdGen, 
                       problemsS :: Problems, 
                       movesS :: Moves,
                       deadEndsS :: DeadEndsMap,
                       solutionsS :: Solutions
                     }
              
initializeSolverFromSeed seed = Solver { boardS = blankBoard, 
                                         genS = mkStdGen seed, 
                                         problemsS = [], n
                                         movesS = [], 
                                         deadEndsS = Map.empty,
                                         solutionsS = [] }
--| Val
data Val = Empty | Num Integer

displayWithHighlighting :: Maybe Square -> Square -> String
displayWithHighlighting Nothing sq = display sq
displayWithHighlighting (Just sqMove) sq | sqMove == sq = color Green (display sq)
                                         | otherwise = display sq


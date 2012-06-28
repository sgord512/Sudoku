module Su.Types where

import Control.Applicative
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Util.List ( chunk )
import Util.String
import System.Random

-- | List of possible numbers that can go in each box. 
nums = [1..9]

-- | Type Synonyms 
type DeadEndsMap = Map Loc [DeadEnd]
type LocValMap = Map Loc Val
type LVPair = (Loc, Val)

type DeadEnds = [DeadEnd]
type Locs = [Loc]
type Moves = [Move]
type Problems = [Problem]              
type Reasons = [Reason]
type Regions = [Region]
type Solutions = [Solution]

-- | Value is used when I know that a particular number is used, as in the case of deadEnds
type Value = Integer

-- | Board 
data Board = Board LocValMap Regions

boardLocValMap :: Board -> LocValMap
boardLocValMap (Board lvMap _) = lvMap 

boardRegions :: Board -> Regions
boardRegions (Board _ r) = r

boardLookupVal :: Board -> Loc -> Val
boardLookupVal b l@(Loc (r, c)) = Map.findWithDefault (error ("Tried to get value of invalid location: " ++ "(" ++ show r ++ "," ++ show c ++ ")")) l (boardLocValMap b)

filled :: Board -> Loc -> Bool
filled b l = case boardLookupVal b l of
  Empty -> False
  Num _ -> True

unfilled b l = not $ filled b l

blankBoard :: Board
blankBoard = let locs = [Loc (row, col) | row <- nums, col <- nums ]
                 regions = nums <**> map (\s n -> Region s n) [Row, Col, Box]
           in Board (Map.fromList $ zip locs (repeat Empty)) regions

-- | DeadEnd
data DeadEnd = DeadEnd Value Problems 
deadEndValue (DeadEnd v _) = v
deadEndProblems (DeadEnd _ ps) = ps

deadEndLookupDeadEnds :: DeadEndsMap -> Loc -> DeadEnds
deadEndLookupDeadEnds dem l = maybe [] id (Map.lookup l dem)


-- | Loc
data Loc = Loc (Integer, Integer) deriving Eq

locRow (Loc (r, _)) = Region Row r
locCol (Loc (_, c)) = Region Col c
locBox (Loc (r, c)) = Region Box (((r - 1) `quot` 3) * 3 + ((c - 1) `quot` 3) + 1) 

locRegions :: Loc -> Regions
locRegions = (<**> [locRow, locCol, locBox]) . pure

instance Ord Loc where 
  (Loc (row, col)) `compare` (Loc (row', col')) = 
    case row `compare` row' of
      EQ -> col `compare` col'
      neq -> neq

-- | Move
data Move = SolutionApplication Solution | RandomMove Loc [Value]

moveLoc :: Move -> Loc                                           
moveLoc (SolutionApplication (Solution l _ _)) = l
moveLoc (RandomMove l _) = l

-- | Problem
data Problem = LocUnfillable Loc | ValueUnmatchable Region Value | LocWithConflictingSolutions Loc [Integer]

-- | Reason
data Reason = OnlyAllowableValue | OnlyLocForValueInRegion Region deriving Eq

-- | Region
data Region = Region Shape Integer deriving Eq

regionShape (Region s _) = s
regionNum (Region _ n) = n

regionLocs :: Region -> Locs
regionLocs (Region Row n) = map (\c -> Loc (n, c)) nums
regionLocs (Region Col n) = map (\r -> Loc (r, n)) nums
regionLocs (Region Box n) = [Loc (r + r', c + c') | r' <- [0,1,2], c' <- [0,1,2] ]
  where r = 3 * ((n - 1) `div` 3) + 1         
        c = 3 * ((n - 1) `mod` 3) + 1

-- | Shape
data Shape = Row | Col | Box deriving (Show, Bounded, Enum, Ord, Eq)

-- | Solution
data Solution = Solution Loc Value Reasons

solutionReasons (Solution _ _ rs) = rs
solutionLoc (Solution l _ _) = l
solutionVal (Solution _ v _) = v

instance Eq Solution where 
         (Solution l v _) == (Solution l' v' _) = (l == l') && (v == v')

-- | Solver
data Solver = Solver { boardS :: Board, 
                       genS :: StdGen, 
                       problemsS :: Problems, 
                       movesS :: Moves,
                       deadEndsS :: DeadEndsMap,
                       solutionsS :: Solutions
                     }
              
initializeSolverFromSeed seed = Solver { boardS = blankBoard, 
                                         genS = mkStdGen seed, 
                                         problemsS = [],
                                         movesS = [], 
                                         deadEndsS = Map.empty,
                                         solutionsS = [] }
-- | Val
data Val = Empty | Num Integer

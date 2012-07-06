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

-- * Type Synonyms 
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

-- * Data Definitions
data Board = Board LocValMap Regions

boardLocValMap :: Board -> LocValMap
boardLocValMap (Board lvMap _) = lvMap 

boardRegions :: Board -> Regions
boardRegions (Board _ r) = r

boardLookupVal :: Board -> Loc -> Val
boardLookupVal b l@(Loc r c) = Map.findWithDefault (error ("Tried to get value of invalid location: " ++ "(" ++ show r ++ "," ++ show c ++ ")")) l (boardLocValMap b)

filled :: Board -> Loc -> Bool
filled b l = case boardLookupVal b l of
  Empty -> False
  Num _ -> True

unfilled b l = not $ filled b l

blankBoard :: Board
blankBoard = let locs = [Loc row col | row <- nums, col <- nums ]
                 regions = nums <**> map (\s n -> Region s n) [Row, Col, Box]
           in Board (Map.fromList $ zip locs (repeat Empty)) regions


-- | This can only result from a 'RandomChoice'
data DeadEnd = DeadEnd Value RandomChoice Problems 

deadEndValue (DeadEnd v _ _) = v
deadEndProblems (DeadEnd _ _ ps) = ps
deadEndRandomChoice (DeadEnd _ rc _) = rc

deadEndLookupDeadEnds :: DeadEndsMap -> Loc -> DeadEnds
deadEndLookupDeadEnds dem l = maybe [] id (Map.lookup l dem)

deadEndAfterRandomChoice :: DeadEnd -> RandomChoice -> Bool
deadEndAfterRandomChoice (DeadEnd _ rc _) rc' = rc == rc'

-- | A location on the sudoku board represented by a row and column
data Loc = Loc Integer Integer deriving Eq

locRow (Loc r _) = Region Row r
locCol (Loc _ c) = Region Col c
locBox (Loc r c) = Region Box (((r - 1) `quot` 3) * 3 + ((c - 1) `quot` 3) + 1) 

locRegions :: Loc -> Regions
locRegions = (<**> [locRow, locCol, locBox]) . pure

instance Ord Loc where 
  (Loc row col) `compare` (Loc row col) = 
    case row `compare` row' of
      EQ -> col `compare` col'
      neq -> neq

-- | Unaesthetically unsymmetric, and should have a unique ID so that a solution can depend on a move.
data Move = SolutionApplication Solution | Branch RandomChoice deriving Eq

moveLoc :: Move -> Loc                                           
moveLoc (SolutionApplication (Solution l _ _)) = l
moveLoc (Branch rc) = randomChoiceLoc rc

moveIsSolutionApplication :: Move -> Bool
moveIsSolutionApplication (SolutionApplication _) = True
moveIsSolutionApplication _ = False

moveIsBranch :: Move -> Bool
moveIsBranch (Branch _) = True
moveIsBranch _ = False

-- | Problem with the current state of the board, forcing the solver to backtrack. Ideally I should be able to tie a problem to a move, so that I know exactly how far to backtrack. 
data Problem = LocUnfillable Loc | ValueUnmatchable Region Value | LocWithConflictingSolutions Loc [Value] deriving (Eq, Ord)

-- | A move that isn't determinstic, but is an arbitrary choice
data RandomChoice = RandomChoice Loc Value [Value]

randomChoiceLoc :: RandomChoice -> Loc
randomChoiceLoc (RandomChoice l _ _) = l

randomChoiceValue :: RandomChoice -> Value
randomChoiceValue (RandomChoice _ v _) = v

randomChoiceValues :: RandomChoice -> [Value]
randomChoiceValues (RandomChoice _ _ vs) = vs

instance Eq RandomChoice where 
  (RandomChoice l v _) == (RandomChoice l' v' _) = l == l' && v == v'

-- | Reason for a particular solution 
data Reason = OnlyAllowableValue | OnlyLocForValueInRegion Region deriving Eq

data Region = Region Shape Integer deriving (Eq, Ord)

regionShape (Region s _) = s
regionNum (Region _ n) = n

regionLocs :: Region -> Locs
regionLocs (Region Row n) = map (\c -> Loc n c) nums
regionLocs (Region Col n) = map (\r -> Loc r n) nums
regionLocs (Region Box n) = [Loc (r + r') (c + c') | r' <- [0,1,2], c' <- [0,1,2] ]
  where r = 3 * ((n - 1) `div` 3) + 1         
        c = 3 * ((n - 1) `mod` 3) + 1

-- | Shape of a region, used only to determine the locations in that region
data Shape = Row | Col | Box deriving (Show, Bounded, Enum, Ord, Eq)

-- | Solution for a location
data Solution = Solution Loc Value Reasons

solutionReasons (Solution _ _ rs) = rs
solutionLoc (Solution l _ _) = l
solutionVal (Solution _ v _) = v

instance Eq Solution where 
         (Solution l v _) == (Solution l' v' _) = (l == l') && (v == v')

-- | This is the piece of data used for the state
data Solver = Solver { boardS :: Board, 
                       genS :: StdGen, 
                       problemsS :: Problems, 
                       movesS :: Moves,
                       deadEndsS :: DeadEndsMap,
                       solutionsS :: Solutions,
                       locToFillS :: Maybe Loc
                     }
-- | Creates the solver used by the program
initializeSolverFromSeed seed = Solver { boardS = blankBoard, 
                                         genS = mkStdGen seed, 
                                         problemsS = [],
                                         movesS = [], 
                                         deadEndsS = Map.empty,
                                         solutionsS = [], 
                                         locToFillS = Nothing }

{-| Val. This is similar to a maybe, but conceptually differs in that Empty is a state of its own, as opposed to the absence of a value. 
    I do have to cast from this whenever I am dealing with filled locs though. 
-}
data Val = Empty | Num Integer

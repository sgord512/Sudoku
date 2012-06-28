module Su.Solver where

import Control.Monad.State
import Data.Map ( Map )
import qualified Data.Map as Map
import Su.Display
import Su.Types ( Board, 
                  DeadEnd, DeadEnds, DeadEndsMap,
                  Loc, Locs,
                  Move, Moves,
                  Problem, Problems,
                  Reason, 
                  Region, Regions, 
                  Shape, 
                  Solution, Solutions, 
                  Solver, 
                  Val, 
                  Value )
import qualified Su.Types as Su

type GameState = State Solver

getBoard :: GameState Board
getBoard = gets Su.boardS
getProblems :: GameState Problems
getProblems = gets Su.problemsS
getMoves :: GameState Moves
getMoves = gets Su.movesS
getDeadEndsMap :: GameState DeadEndsMap
getDeadEndsMap = gets Su.deadEndsS
getSolutions :: GameState Solutions
getSolutions = gets Su.solutionsS
getLocs :: GameState Locs
getLocs = do
  b <- getBoard
  return $ Map.keys $ Su.boardLocValMap b
getRegions :: GameState Regions
getRegions = do    
  b <- getBoard
  return $ Su.boardRegions b


filled :: Loc -> GameState Bool
filled l = do
  b <- getBoard 
  return $ Su.filled b l

unfilled l = do 
  b <- getBoard 
  return $ not $ Su.filled b l
            
-- | Partial function that allows for type conversion betwen Values with constructor Num and Integer            
valNum :: Su.Val -> Integer
valNum Su.Empty = error "Can't call valNum on an empty value"
valNum (Su.Num n) = n

filledValue :: Loc -> GameState Value
filledValue l = do 
  b <- getBoard 
  return $ valNum $ Su.boardLookupVal b l

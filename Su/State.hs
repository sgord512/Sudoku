module Su.State where

import Control.Monad.Identity
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
import System.Random

type SolverStateT = StateT Solver
type GameState = SolverStateT Identity

getBoard :: Monad m => SolverStateT m Board
getBoard = gets Su.boardS

getProblems :: Monad m => SolverStateT m Problems
getProblems = gets Su.problemsS

getMoves :: Monad m => SolverStateT m Moves
getMoves = gets Su.movesS

getDeadEndsMap :: Monad m => SolverStateT m DeadEndsMap
getDeadEndsMap = gets Su.deadEndsS

getSolutions :: Monad m => SolverStateT m Solutions
getSolutions = gets Su.solutionsS

getLocToFill :: Monad m => SolverStateT m (Maybe Loc)
getLocToFill = gets Su.locToFillS

getLocs :: Monad m => SolverStateT m Locs
getLocs = do
  b <- getBoard
  return $ Map.keys $ Su.boardLocValMap b

getRegions :: Monad m => SolverStateT m Regions
getRegions = do    
  b <- getBoard
  return $ Su.boardRegions b

getRandomGen :: Monad m => SolverStateT m StdGen
getRandomGen = gets Su.genS

getSolver :: Monad m => SolverStateT m Solver
getSolver = get             



filled :: Monad m =>  Loc -> SolverStateT m Bool
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

filledValue :: Monad m => Loc -> SolverStateT m Value
filledValue l = do 
  b <- getBoard 
  return $ valNum $ Su.boardLookupVal b l

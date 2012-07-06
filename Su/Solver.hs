{-# LANGUAGE RecordWildCards #-}
module Su.Solver where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe ( fromJust )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment
import System.Random
import Su.Display
import Su.State
import Su.Types hiding ( filled, unfilled )
import Util.Display
import Util.List
import Util.Monad

-- | Pick a random element of a list, using 'randomElement'
randomElem :: Monad m => [a] -> SolverStateT m a
randomElem xs = do 
  gen <- getRandomGen
  let (x, gen') = randomElement gen xs
  modify (\solver -> solver { genS = gen' }) 
  return x


-- | Candidate values for a location. Assumes the location is unfilled, and I'm not sure what it will do if this is not the case. 
possibilities :: Monad m => Loc -> SolverStateT m [Value]
possibilities l = do 
  let locs = concat $ map regionLocs (locRegions l)
  filledLocs <- filterM filled locs
  filledValues <- mapM filledValue filledLocs
  let prelimPoss = nums \\ nub filledValues
  devs <- deadEndValues l
  return $ prelimPoss \\ devs

-- | Gets the values that are known to be dead ends for a particular location
deadEndValues :: Monad m => Loc -> SolverStateT m [Value]
deadEndValues l = do
  dem <- getDeadEndsMap
  return $ map deadEndValue (deadEndLookupDeadEnds dem l)
  
-- | Does the location have exactly one possible value?
solvable :: Monad m => Loc -> SolverStateT m Bool
solvable l = do 
  poss <- possibilities l
  return $ length poss == 1
  
-- | /This is not the negation of solvable/. This answers \"Are there no possible values for this location?\"
unsolvable :: Monad m => Loc -> SolverStateT m Bool  
unsolvable l = do
  poss <- possibilities l
  return $ null poss
  
-- * Getting solutions  
  
-- | Returns solutions for locations with exactly one possible value
allLocSolutions :: Monad m => SolverStateT m Solutions
allLocSolutions = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  filterMapM locSolution unfilledLocs
  where locSolution :: Monad m => Loc -> SolverStateT m (Maybe Solution)  
        locSolution l = do 
          poss <- possibilities l
          case poss of
            (n:[]) -> return $ Just $ Solution l n [OnlyAllowableValue]
            _ -> return Nothing
          
-- | Solutions for a region
regionSolutions :: Monad m => Region -> SolverStateT m [Solution]
regionSolutions r = do 
  valuesLocs <- regionUnmatchedValuesAndLocs r
  return $ filterMap valueSolution valuesLocs
  where valueSolution (v, (l:[])) = Just $ Solution l v [OnlyLocForValueInRegion r]
        valueSolution (v, _) = Nothing 

-- | All solutions due to constraints on regions  
allRegionSolutions :: Monad m => SolverStateT m [Solution]  
allRegionSolutions = do
  rgns <- getRegions
  rgnSolutions <- mapM regionSolutions rgns
  return $ foldr1 (\a b -> union a b) rgnSolutions
  
-- | All solutions: Comprised of solutions due to regions and locations
allSolutions :: Monad m => SolverStateT m [Solution]
allSolutions = do
  locSolutions <- allLocSolutions
  regSolutions <- allRegionSolutions
  let solnPairs = map (\soln -> (solutionLoc soln, soln)) (locSolutions ++ regSolutions)
      mergeSolutions = (\s s' -> Solution (solutionLoc s) (solutionVal s) (solutionReasons s ++ solutionReasons s'))
      solnMap = Map.fromListWith mergeSolutions solnPairs
  return $ snd $ unzip $ Map.toList solnMap

-- * Machinery for finding solutions in a region

-- | The values that are not placed in a location in the provided region  
regionUnmatchedValues :: Monad m => Region -> SolverStateT m [Value]
regionUnmatchedValues r = do
  filledLocs <- filterM filled (regionLocs r) 
  filledValues <- mapM filledValue filledLocs
  return $ nums \\ filledValues
  
-- | Can this value be placed at this location?  
candidateValueFor :: Monad m => Value -> Loc -> SolverStateT m Bool
n `candidateValueFor` l = do
  poss <- possibilities l
  return $ n `elem` poss
  
-- | Unmatched values in a region and possible locations for those values  
regionUnmatchedValuesAndLocs :: Monad m => Region -> SolverStateT m [(Value, Locs)]
regionUnmatchedValuesAndLocs r = do
  emptyLocs <- filterM unfilled (regionLocs r)
  values <- regionUnmatchedValues r
  let possibleLocsForValue = (\n -> filterM (n `candidateValueFor`) emptyLocs)
  (liftM $ zip values) (mapM possibleLocsForValue values)

-- * Problems
-- $problem When searching the solution space, a wrong move may lead to a board that is unsolvable, which is represented by one of a variety of different types of problems

-- ** Different types of problems
-- | Problems due to locations that cannot be filled
unfillableLocProblems :: Monad m => SolverStateT m Problems
unfillableLocProblems = do  
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  problemLocs <- filterM unsolvable unfilledLocs
  return $ map LocUnfillable problemLocs

-- | Problems due to locations that must contain multiple values
multipleSolutionProblems :: Monad m => SolverStateT m Problems
multipleSolutionProblems = do
  solns <- allSolutions
  board <- getBoard
  overwritingSolns <- filterM (\soln -> filled (solutionLoc soln)) solns
  overwrittenValues <- mapM (\soln -> filledValue (solutionLoc soln)) overwritingSolns
  return $ zipWith (\soln value -> LocWithConflictingSolutions (solutionLoc soln) [solutionVal soln, value]) overwritingSolns overwrittenValues
  
-- | Problems with values that cannot be matched with any locations in regions
unmatchableValueProblems :: Monad m => SolverStateT m Problems
unmatchableValueProblems = do
  regions <- getRegions
  unmatchableValueProbs <- (liftM concat) ((mapM regionValueProblems) regions)
  return unmatchableValueProbs
  
-- *** Detects an unmatchable value in a particular region. Used above
-- | Problems due to values being unmatchable in a region
regionValueProblems :: Monad m => Region -> SolverStateT m [Problem]
regionValueProblems r = do
  vls <- regionUnmatchedValuesAndLocs r
  return $ filterMap problemValue vls
  where problemValue (v, []) = Just $ ValueUnmatchable r v
        problemValue (_, _) = Nothing 
        
-- ** All problems
-- | All current problems
findProblems :: Monad m => SolverStateT m [Problem]
findProblems = do
  problems' <- unfillableLocProblems
  problems'' <- multipleSolutionProblems
  problems''' <- unmatchableValueProblems
  return $ Set.toList $ Set.unions [Set.fromList problems', Set.fromList problems'', Set.fromList problems''']
        
-- | Update the list of problems to include all current problems
updateProblems :: Monad m => SolverStateT m [Problem]    
updateProblems = do
  newProbs <- findProblems
  oldProbs <- getProblems
  let allProbs = newProbs ++ oldProbs      
  modify (\solver -> solver { problemsS = allProbs })
  return allProbs
  
-- | Empty the list of known problems
clearProblems :: Monad m => SolverStateT m ()  
clearProblems = modify (\solver -> solver { problemsS = [] })

-- * Filling locations
-- ** Two options:   
-- | Fill a random location with one of its possible values
fillRandomLoc :: Monad m => SolverStateT m Move
fillRandomLoc = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  loc <- randomElem unfilledLocs
  fillLocRandomly loc
    
    
-- | Given a loc, randomly choose one of its possible values, and fill it with that value     
fillLocRandomly :: Monad m => Loc -> SolverStateT m Move
fillLocRandomly loc = do
  poss <- possibilities loc
  value <- randomElem poss
  loc' <- updateLoc loc (Num value)
  return $ Branch $ RandomChoice loc' value poss
    
-- | Apply a solution    
fillSolvedLoc :: Monad m => Solution -> SolverStateT m Move
fillSolvedLoc soln@(Solution l v r) = do 
  updateLoc l (Num v)
  return $ SolutionApplication soln
  
-- ** Actually fill the location with a provided value                                                              
-- | Update a location with a value
updateLoc :: Monad m => Loc -> Val -> SolverStateT m Loc 
updateLoc l v = modify (\solver@Solver{ boardS = (Board map regions) } -> solver { boardS = Board (Map.insert l v map) regions }) >> return l

-- * Core game logic
-- | Are there solutions at the moment?
areSolutions :: Monad m => SolverStateT m Bool
areSolutions = gets (not . null . solutionsS)
    
-- | Update the list of solutions with current solutions               
updateSolutions :: Monad m => SolverStateT m ()
updateSolutions = do
  solns <- allSolutions
  modify (\s -> s { solutionsS = solns })

-- | Are there currently empty locations?
boardUnfilled :: Monad m => SolverStateT m Bool
boardUnfilled = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  return $ not $ null unfilledLocs
         

-- | Fill a random location and record that move 
branchRandomlyAndRecordMove :: Monad m => SolverStateT m Move
branchRandomlyAndRecordMove = fillRandomLoc >>= recordMove

-- | Fill a certain location with one of its possibilities and record that move
branchAtLocAndRecordMove :: Monad m => Loc -> SolverStateT m Move
branchAtLocAndRecordMove loc = fillLocRandomly loc >>= recordMove 

-- | Apply a solution and record that move
solveAndRecordMove :: Monad m => SolverStateT m Move
solveAndRecordMove = do (soln:solns) <- gets solutionsS  
                        modify (\s -> s { solutionsS = solns })
                        fillSolvedLoc soln >>= recordMove

          
-- | Record a move by adding it the stack of moves
recordMove :: Monad m => Move -> SolverStateT m Move
recordMove m = modify (\s@Solver{ movesS = moves } -> s { movesS = (m:moves) }) >> return m
    
-- | Find the most recent move that involved a branch on a random choice
mostRecentBranch :: Monad m => SolverStateT m (Maybe Move)
mostRecentBranch = do
  moves <- getMoves
  return $ find moveIsBranch moves

-- | Backtrack to the last nondeterministic move, and mark that move as a dead end    
backtrack :: Monad m => Problems -> SolverStateT m (Loc, DeadEnd)
backtrack probs = do
  moves <- getMoves
  let (_, (branch@(Branch randChoice)):validMoves) = break moveIsBranch moves
      loc = moveLoc branch
  value <- filledValue loc
  updateLoc loc Empty
  des <- pruneOldDeadEnds randChoice
  
  let de = DeadEnd value randChoice probs
  modify (\solver@Solver { deadEndsS = deadEnds } -> solver { deadEndsS = Map.insertWith (++) loc [de] deadEnds,
                                                              movesS = validMoves, 
                                                              solutionsS = [],
                                                              problemsS = [],
                                                              locToFillS = Just loc })
  return (loc, de)
  
{-- The problem is that I need to clear the last move during the backtracking or else I will end up having each move dependent on itself, which makes no sense.  
--}
  
backtrackIO :: Problems -> SolverStateT IO (Loc, DeadEnd)  
backtrackIO probs = do
  moves <- getMoves
  let (branch, validMoves) = getLastBranchAndPriorMoves moves
      (prevBranch@(Branch randChoice), _) = getLastBranchAndPriorMoves validMoves
      loc = moveLoc prevBranch
  value <- filledValue loc
  updateLoc loc Empty
  removedDeadEnds <- pruneOldDeadEnds randChoice
  lift $ putStrLn "Removing the following dead ends!!!" >> putStrLn (unlines $ map display removedDeadEnds)
  let de = DeadEnd value randChoice probs
  modify (\solver@Solver { deadEndsS = deadEnds } -> solver { deadEndsS = Map.insertWith (++) loc [de] deadEnds,
                                                              movesS = validMoves, 
                                                              solutionsS = [],
                                                              problemsS = [],
                                                              locToFillS = Just loc })
  return (loc, de)  
  
getLastBranchAndPriorMoves :: Moves -> (Move, Moves)
getLastBranchAndPriorMoves moves = let (_, branch:validMoves) = break moveIsBranch moves
                                   in (branch, validMoves)

-- | Get rid of dead ends that no longer apply
pruneOldDeadEnds :: Monad m => RandomChoice -> SolverStateT m DeadEnds
pruneOldDeadEnds randChoice = do
  dem <- getDeadEndsMap
  let (oldDeadEnds, dem') = Map.mapAccum takeOldDeadEnds [] dem 
  modify (\solver -> solver { deadEndsS = dem' })
  return oldDeadEnds
  where takeOldDeadEnds odes des = let (oldDEs, currentDEs) = partition (`deadEndAfterRandomChoice` randChoice) des
                                   in (odes ++ oldDEs, currentDEs)
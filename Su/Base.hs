{-# LANGUAGE RecordWildCards #-}
module Su.Base where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe ( fromJust )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import System.Random
import Su.Display
import Su.Solver 
import Su.Types hiding ( filled, unfilled )
import Util.List
import Util.Monad

rGen f = do
 state <- get
 let (a, gen') = f $ genS state
 put $ state { genS = gen' }
 return a

randomElement :: [a] -> GameState a
randomElement xs = do 
  ix <- rGen $ randomR (0, length xs - 1)
  return (xs !! ix)

possibilities :: Loc -> GameState [Value]
possibilities l = do 
  let locs = concat $ map regionLocs (locRegions l)
  filledLocs <- filterM filled locs
  filledValues <- mapM filledValue filledLocs
  let prelimPoss = nums \\ nub filledValues
  devs <- deadEndValues l
  return $ prelimPoss \\ devs

deadEndValues :: Loc -> GameState [Value]
deadEndValues l = do
  dem <- getDeadEndsMap
  return $ map deadEndValue (deadEndLookupDeadEnds dem l)
  
isSolvable :: Loc -> GameState Bool
isSolvable l = do 
  poss <- possibilities l
  return $ length poss == 1
  
isUnsolvable :: Loc -> GameState Bool  
isUnsolvable l = do
  poss <- possibilities l
  return $ null poss
  
allLocSolutions :: GameState Solutions
allLocSolutions = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  filterMapM locSolution unfilledLocs
  where locSolution :: Loc -> GameState (Maybe Solution)  
        locSolution l = do 
          poss <- possibilities l
          case poss of
            (n:[]) -> return $ Just $ Solution l n [OnlyAllowableValue]
            _ -> return Nothing
  
regionUnmatchedValues :: Region -> GameState [Value]
regionUnmatchedValues r = do
  filledLocs <- filterM filled (regionLocs r) 
  filledValues <- mapM filledValue filledLocs
  return $ nums \\ filledValues
  
candidateValueFor :: Value -> Loc -> GameState Bool
n `candidateValueFor` l = do
  poss <- possibilities l
  return $ n `elem` poss
  
valuesAndLocs :: Region -> GameState [(Value, Locs)]
valuesAndLocs r = do
  emptyLocs <- filterM unfilled (regionLocs r)
  values <- regionUnmatchedValues r
  let possibleLocsForValue = (\n -> filterM (n `candidateValueFor`) emptyLocs)
  (liftM $ zip values) (mapM possibleLocsForValue values)
          
regionSolutions :: Region -> GameState [Solution]
regionSolutions r = do 
  valuesLocs <- valuesAndLocs r
  return $ filterMap valueSolution valuesLocs
  where valueSolution (v, (l:[])) = Just $ Solution l v [OnlyLocForValueInRegion r]
        valueSolution (v, _) = Nothing 

regionValueProblems :: Region -> GameState [Problem]
regionValueProblems r = do
  vls <- valuesAndLocs r
  return $ filterMap problemValue vls
  where problemValue (v, []) = Just $ ValueUnmatchable r v
        problemValue (_, _) = Nothing 

fillRandomLoc :: GameState Move
fillRandomLoc = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  loc <- randomElement unfilledLocs
  poss <- possibilities loc
  value <- randomElement poss
  loc' <- updateLoc loc (Num value)
  return $ RandomMove loc' poss
  
allRegionSolutions :: GameState [Solution]  
allRegionSolutions = do
  rgns <- getRegions
  rgnSolutions <- mapM regionSolutions rgns
  return $ foldr1 (\a b -> union a b) rgnSolutions
  
allSolutions :: GameState [Solution]
allSolutions = do
  locSolutions <- allLocSolutions
  regSolutions <- allRegionSolutions
  let solnPairs = map (\soln -> (solutionLoc soln, soln)) (locSolutions ++ regSolutions)
      mergeSolutions = (\s s' -> Solution (solutionLoc s) (solutionVal s) (solutionReasons s ++ solutionReasons s'))
      solnMap = Map.fromListWith mergeSolutions solnPairs
  return $ snd $ unzip $ Map.toList solnMap

areSolutions :: GameState Bool
areSolutions = gets (not . null . solutionsS)
    
updateSolutions :: GameState ()
updateSolutions = do
  solns <- allSolutions
  modify (\s -> s { solutionsS = solns })
    
fillSolvedLoc :: Solution -> GameState Move
fillSolvedLoc soln@(Solution l v r) = do 
  updateLoc l (Num v)
  return $ SolutionApplication soln
                                                              
updateLoc :: Loc -> Val -> GameState Loc 
updateLoc l v = modify (\solver@Solver{ boardS = (Board map regions) } -> solver { boardS = Board (Map.insert l v map) regions }) >> return l
  
boardUnfilled :: GameState Bool
boardUnfilled = do 
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  return $ not $ null unfilledLocs

                 
solveIO :: Solver -> IO Solver         
solveIO solver = do
  let (continue, solver') = runState boardUnfilled solver
      (probs, solver'') = runState problems solver'
  if continue
    then if null probs
         then do solver''' <- oneStepIO solver''
                 putStrLn $ display solver'''
                 solveIO solver'''
         else do mapM_ (putStrLn . display) probs
                 let solver''' = execState (markDeadEnd >> undoLastMove) solver'' 
                 solveIO solver'''
    else do putStrLn "Done!" >> return solver''
          
oneStepIO solver = do              
  let (solutionsWaiting, solver') = runState areSolutions solver
      (solns, solver'') = runState getSolutions (if solutionsWaiting then solver' else (execState updateSolutions solver'))
  mapM_ (putStrLn . display) solns
  return $ execState (if null solns then randomAndRecordMove else solveAndRecordMove) solver''
              
solve :: GameState Solver
solve = do 
  continue <- boardUnfilled
  if continue      
     then do probs <- problems 
             if null probs 
               then do oneStep 
                       solve
               else do markDeadEnd
                       undoLastMove
                       solve
    else get 

randomAndRecordMove :: GameState Move
randomAndRecordMove = fillRandomLoc >>= recordMove

solveAndRecordMove :: GameState Move
solveAndRecordMove = do (soln:solns) <- gets solutionsS  
                        modify (\s -> s { solutionsS = solns })
                        fillSolvedLoc soln >>= recordMove

oneStep = do
  solutionsWaiting <- areSolutions
  unless solutionsWaiting updateSolutions
  solns <- getSolutions
  move <- case solns of
    [] -> fillRandomLoc              
    x:xs -> fillSolvedLoc x               
  recordMove move
          
recordMove :: Move -> GameState Move
recordMove m = modify (\s@Solver{ movesS = moves } -> s { movesS = (m:moves) }) >> return m
    
markDeadEnd :: GameState ()
markDeadEnd = do 
  solvr <- get
  let loc = moveLoc $ head $ movesS solvr
      problems = problemsS solvr
  value <- filledValue loc
  let de = DeadEnd value problems
  modify (\solver@Solver { deadEndsS = deadEnds } -> solver { deadEndsS = Map.insertWith (++) loc [de] deadEnds })
  
undoLastMove :: GameState ()
undoLastMove = do
  (m:ms) <- gets movesS
  let l = moveLoc m
  updateLoc l Empty
  modify (\s -> s { movesS = ms, solutionsS = [] })
               
stepBoard :: Solver -> Solver
stepBoard solver = execState oneStep solver

unfillableLocProblems :: GameState Problems
unfillableLocProblems = do  
  locs <- getLocs
  unfilledLocs <- filterM unfilled locs
  problemLocs <- filterM isUnsolvable unfilledLocs
  return $ map LocUnfillable problemLocs

multipleSolutionProblems :: GameState Problems
multipleSolutionProblems = do
  solns <- allSolutions
  board <- getBoard
  overwritingSolns <- filterM (\soln -> filled (solutionLoc soln)) solns
  overwrittenValues <- mapM (\soln -> filledValue (solutionLoc soln)) overwritingSolns
  return $ zipWith (\soln value -> LocWithConflictingSolutions (solutionLoc soln) [solutionVal soln, value]) overwritingSolns overwrittenValues

problems :: GameState [Problem]
problems = do
  problems' <- unfillableLocProblems
  problems'' <- multipleSolutionProblems
  problems''' <- unmatchableValueProblems
  return $ problems' ++ problems'' ++ problems'''
  
  
unmatchableValueProblems :: GameState Problems
unmatchableValueProblems = do
  regions <- getRegions
  unmatchableValueProbs <- (liftM concat) ((mapM regionValueProblems) regions)
  return unmatchableValueProbs


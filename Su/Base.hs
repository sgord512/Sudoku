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
import Su.Types
import Util.List
import Util.Monad

type GameState = State Solver

rGen f = do state <- get
            let (a, gen') = f $ genS state
            put $ state { genS = gen' }
            return a

randomElement :: [a] -> GameState a
randomElement xs = do 
  ix <- rGen $ randomR (0, length xs - 1)
  return (xs !! ix)

possibilities :: Square -> GameState [Integer]
possibilities s = do 
  sqs <- liftM concat $ mapM squaresInRegion (regions s)
  let prelimPoss = nums \\ (nub $ map val (filter filled sqs))
  des <- deadEnds s
  return $ prelimPoss \\ des

deadEnds :: Square -> GameState [Integer]
deadEnds s = do
  allDeadEnds <- gets ((Map.lookup (loc s)) . deadEndsS)
  let dEnds = case allDeadEnds of 
        Nothing -> []
        Just des -> map (\(DeadEnd s i _) -> i) des
  return dEnds

isSolvable :: Square -> GameState Bool
isSolvable s = do 
  poss <- possibilities s
  return (length poss == 1)
  
isUnsolvable :: Square -> GameState Bool  
isUnsolvable s = do
  poss <- possibilities s
  return $ null poss
  
allSquareSolutions :: GameState [Solution]
allSquareSolutions = do squares <- gets (allSquares . boardS) 
                        let possSquares = filter unfilled squares
                        filterMapM hasSolution possSquares
                        
hasSolution :: Square -> GameState (Maybe Solution)  
hasSolution sq = do 
  poss <- possibilities sq
  case poss of
    (n:[]) -> return $ Just $ Solution (Square (loc sq) (Just n)) [OnlyAllowableValue]
    _ -> return Nothing

squaresInRegion :: Region -> GameState [Square]
squaresInRegion r@(Region s i) = do 
  squares <- gets (allSquares . boardS)
  return $ filter ((== r) . (getShapeFunc s)) squares
  
numbersToMatch :: Region -> GameState [Integer]
numbersToMatch r = do
  sqs <- squaresInRegion r
  return $ nums \\ (map val (filter filled sqs))
  
isPossibleValueFor :: Integer -> Square -> GameState Bool
i `isPossibleValueFor` s = do
  poss <- possibilities s
  return $ i `elem` poss
  
squaresForNumbers :: Region -> GameState [(Integer, [Square])]
squaresForNumbers r = do
  sqs <- squaresInRegion r
  let emptySquares = filter unfilled sqs  
      numsToMatch = nums \\ (map val (filter filled sqs))
  (liftM $ zip numsToMatch) (forM numsToMatch (\n -> filterM (n `isPossibleValueFor`) emptySquares))

regionSolutions :: Region -> GameState [Solution]
regionSolutions r = do 
  sqWithNums <- squaresForNumbers r
  return $ filterMap (\(n, sqs) -> if length sqs == 1 
                                   then Just $ Solution (Square (loc (head sqs)) (Just n)) [OnlyLocationForNumberInRegion r]
                                   else Nothing) sqWithNums

unmatchableNumbers :: Region -> GameState [Problem]
unmatchableNumbers r = do
  sqsWithNums <- squaresForNumbers r
  return $ filterMap (\(n, sqs) -> if null sqs then Just $ NumberUnmatchable r n else Nothing) sqsWithNums

fillRandomSquare :: GameState Move
fillRandomSquare = do 
  sq <- (liftM $ filter unfilled) (gets $ allSquares . boardS) >>= randomElement
  poss <- possibilities sq
  value <- randomElement poss
  sq' <- updateSquare $ Square (loc sq) (Just value)
  return $ RandomMove sq' poss
  
allRegionSolutions :: GameState [Solution]  
allRegionSolutions = do
  rgns <- gets (allRegions . boardS)
  rgnSolutions <- mapM regionSolutions rgns
  return $ foldr1 (\a b -> union a b) rgnSolutions
  
allSolutions :: GameState [Solution]
allSolutions = do
  sqSolutions <- allSquareSolutions
  reSolutions <- allRegionSolutions
  let solnPairs = map (\soln@(Solution sq _) -> (sq, soln)) (sqSolutions ++ reSolutions)
  return $ (snd . unzip) $ Map.toList $ Map.fromListWith (\soln soln' -> Solution (solutionSquare soln) (reasons soln ++ reasons soln')) solnPairs
        
fillSolvedSquare :: Solution -> GameState Move
fillSolvedSquare soln@(Solution s _) = do updateSquare s 
                                          return $ SolutionApplication soln
                                                              
updateSquare :: Square -> GameState Square
updateSquare sq = modify (\solver@Solver { boardS = (Board map regions) } -> solver { boardS = Board (Map.insert (loc sq) sq map) regions }) >> return sq
  
boardUnfilled = do 
  sqs <- gets (allSquares . boardS)
  return $ not $ null (filter unfilled sqs)

                 
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
  let (solns, solver') = runState allSolutions solver
  mapM_ print solns
  let (move, solver'') = case solns of
        [] -> runState fillRandomSquare solver'
        soln:solns -> runState (fillSolvedSquare soln) solver'
  return $ execState (recordMove move) solver''
              
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

oneStep = do
  solns <- allSolutions
  move <- case solns of
    [] -> fillRandomSquare              
    x:xs -> fillSolvedSquare x               
  recordMove move
          
recordMove :: Move -> GameState Move
recordMove m = modify (\s@Solver{ movesS = moves } -> s { movesS = (m:moves) }) >> return m
    
markDeadEnd :: GameState ()
markDeadEnd = do 
  s <- get
  let sq = (squareInMove $ head $ movesS s)
      problems = problemsS s
      de = DeadEnd sq (val sq) problems
  modify (\solver@Solver { deadEndsS = deadEnds } -> solver { deadEndsS = Map.insertWith (++) (loc sq) [de] deadEnds })
  
undoLastMove :: GameState ()
undoLastMove = do
  (m:ms) <- gets movesS
  let sq = squareInMove m
  updateSquare (Square (loc sq) Nothing)
  modify (\s -> s { movesS = ms })
  
               
stepBoard :: Solver -> Solver
stepBoard solver = execState oneStep solver

problems :: GameState [Problem]
problems = do
  sqs <- (liftM (filter unfilled)) (gets (allSquares . boardS))
  sqs' <- filterMapM (\s -> do unsolvable <- isUnsolvable s; if unsolvable then return $ Just (SquareUnfillable s) else return Nothing) sqs
  multipleSolns <- filterMapM (\(Solution sq _) -> do squareMap <- gets (squaresMap . boardS)
                                                      let actualSquare = fromJust $ Map.lookup (loc sq) squareMap
                                                      if filled actualSquare 
                                                        then return $ Just (SquareWithConflictingSolutions sq (nub [val sq, val actualSquare]))
                                                        else return Nothing) =<< allSolutions
  rgns <- gets (allRegions . boardS)
  unmatchableNums <- (liftM concat) ((mapM unmatchableNumbers) rgns)
  return $ sqs' ++ unmatchableNums ++ multipleSolns


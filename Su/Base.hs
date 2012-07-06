module Su.Base where

import Control.Monad
import Control.Monad.State
import Su.Display
import Su.Types
import Su.Solver
import Su.State
import Util.Display
import Util.String

doSolveIO :: Solver -> IO Solver
doSolveIO solver = execStateT solveIO solver

{-- 
Here is the problem!!! 
Basically, I mark dead ends whenever I run into a problem, but I keep them around even if the circumstances under which the move is a dead end have changed. 
So if I have each dead end dependent upon a previous random move, I can then clean them up when they are stale, and everything should work well.

Well, that didn't work, and I think the reason is that the pruning is done incorrectly, in which case, the same problems can occur over and over again. 
You can fix it, Spencer. 

So the problem at the moment is that it won't backtrack sufficiently, by which I mean that if all of the possibilities with the last branch are dead ends, it won't mark that branch a dead end, and try another.

Run it like it is with: "./sudoku -vs 100" to see the error, which should be clear from that. 

step :: SolverStateT IO Solver
step = do 
  continue <- boardUnfilled  
  if not continue
     then getSolver
     else do probs <- findProblems
             if null probs
               then do fillOneIO                       
                       getSolver
               else do de@(loc, deadEnd) <- backtrack probs
                       fillLocRandomly loc
                       dispSolver
                       getSolver

go :: Solver -> IO Solver
go solver = do execStateT step solver
--}

solveIO :: SolverStateT IO Solver        
solveIO = do
  continue <- boardUnfilled  
  if not continue
     then getSolver
     else do probs <- updateProblems 
             if null probs                
               then do fillOneIO
                       dispSolver     
                       solveIO
               else do dispSolver
                       de@(loc, deadEnd) <- backtrackIO probs
                       dispSolver
                       solveIO
                        
dispSolver :: SolverStateT IO ()            
dispSolver = getSolver >>= lift . disp
                      
            
fillOneIO :: SolverStateT IO Solver
fillOneIO = do
   solutionsWaiting <- areSolutions
   when solutionsWaiting updateSolutions
   solns <- getSolutions
   lift $ mapM_ disp solns
   move <- if null solns
           then do 
             locToFill <- getLocToFill
             case locToFill of 
               Nothing -> branchRandomlyAndRecordMove               
               Just loc -> do 
                 m <- branchAtLocAndRecordMove loc
                 modify (\solver -> solver { locToFillS = Nothing })
                 return m
           else solveAndRecordMove
   lift $ putStrLn $ (color Green "Move") ++ ": " ++ display move
   getSolver
  
doSolve = undefined

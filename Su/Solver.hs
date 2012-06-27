module Su.Solver where

import Control.Monad.State
import Su.Display
import Su.Types ( Board, 
                  DeadEnd, 
                  Loc, 
                  Move, 
                  Problem, 
                  Reason, 
                  Region, 
                  Shape, 
                  Solution, 
                  Solver, 
                  Val )
import qualified Su.Types as Su

type GameState = State Solver

getBoard = gets . Su.boardS
getProblems = gets . Su.problemsS
getMoves = gets . Su.movesS
getDeadEnds = gets . Su.deadEndsS
getSolutions = gets . Su.solutionS

filled = do getBoard >>= return . Su.filled 
            
--| Partial function that allows for type conversion betwen Values with constructor Num and Integer            
valNum :: Val -> Integer
valNum Empty = error "Can't call valNum on an empty value"
valNum (Num n) = n

filledVal = do getBoard >>= return . valNum . Su.filled 
            

module Main where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe ( fromJust )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Random
import Toolbox

data Solver = Solver { boardS :: Board, 
                       genS :: StdGen, 
                       problemsS :: Problems, 
                       movesS :: Squares
                     }

type Problems = [Problem]              
type GameState = State Solver
type Solution = (Square, Integer)

initialState seed = Solver { boardS = newBoard, genS = mkStdGen seed, problemsS = [], movesS = [] }

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
  return $ nums \\ (nub $ map val (filter filled sqs))

isSolvable :: Square -> GameState Bool
isSolvable s = do 
  poss <- possibilities s
  return (length poss == 1)
  
isUnsolvable :: Square -> GameState Bool  
isUnsolvable s = do
  poss <- possibilities s
  return $ null poss
  
squareSolutions :: GameState [Solution]
squareSolutions = do gets (allSquares . boardS) >>= (filterMapM solutionOrNothing)
  where solutionOrNothing s = do 
          poss <- possibilities s
          case poss of
            (n:[]) -> return $ Just (s, n)
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
  return $ filterMap (\(n, sqs) -> if length sqs == 1 then Just (head sqs, n) else Nothing) sqWithNums

unmatchableNumbers :: Region -> GameState [Problem]
unmatchableNumbers r = do
  sqsWithNums <- squaresForNumbers r
  return $ filterMap (\(n, sqs) -> if null sqs then Just $ NumberUnmatchable r n else Nothing) sqsWithNums

fillRandomSquare :: GameState Square
fillRandomSquare = do 
  sq <- (liftM $ filter unfilled) (gets $ allSquares . boardS) >>= randomElement
  value <- possibilities sq >>= randomElement 
  sq' <- fillSquare sq value
  return sq'
  
allRegionSolutions :: GameState [Solution]  
allRegionSolutions = do
  rgns <- gets (allRegions . boardS)
  rgnSolutions <- mapM regionSolutions rgns
  return $ foldr1 (\a b -> union a b) rgnSolutions
  
allSolutions :: GameState [Solution]
allSolutions = do
  sqSolutions <- squareSolutions
  reSolutions <- allRegionSolutions
  return $ union sqSolutions reSolutions

fillSquare :: Square -> Integer -> GameState Square
fillSquare s v = do 
  let sq = Square (Just v) (loc s)
  modify (\solver@Solver { boardS = (Board map regions), movesS = moves } -> solver { boardS = Board (Map.insert (loc s) sq map) regions, movesS = (sq:moves) }) >> return s
  
newBoard :: Board
newBoard = let squares = [Square Nothing (Loc (row, col)) | row <- nums, col <- nums ]
               regions = nums <**> (map (\s n -> Region s n) (map fst shapeFuncs))
           in Board (Map.fromList $ zip (map loc squares) squares) regions
              
boardUnfilled = do 
  sqs <- gets (allSquares . boardS)
  return $ not $ null (filter unfilled sqs)

solve :: GameState ()
solve = do 
  inProgress <- boardUnfilled
  if inProgress      
     then do areProblems <- problems 
             if areProblems 
               then return ()
               else do oneStep
                       solve
    else return ()
                 
solveIO :: Solver -> IO ()         
solveIO solver  = do
  if evalState boardUnfilled solver 
     then 
  
         
oneStep = do
  solns <- allSolutions
  case solns of
    [] -> fillRandomSquare
    x:xs -> (uncurry fillSquare) x               
          
stepBoard :: Solver -> Solver
stepBoard solver = execState oneStep solver

problems :: GameState Bool
problems = do
  sqs <- (liftM (filter unfilled)) (gets (allSquares . boardS))
  sqs' <- filterMapM (\s -> do unsolvable <- isUnsolvable s; if unsolvable then return $ Just (SquareUnfillable s) else return Nothing) sqs
  rgns <- gets (allRegions . boardS)
  unmatchableNums <- (liftM concat) ((mapM unmatchableNumbers) rgns)
  get >>= (\solver@Solver { problemsS = problems } -> put $ solver { problemsS = problems ++ sqs' })
  p <- gets problemsS 
  return (not $ null p)

seedValue = 100

main = do 
--  seed <- getStdRandom random :: IO Int
  seed <- return seedValue
  putStrLn ("Seed is: " ++ show seed)
  let solver = execState fillBoard (initialState seed)
  putStrLn (display $ boardS solver)

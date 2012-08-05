module Su.Solver where

import Control.Monad
import Data.List
import Data.Maybe ( fromJust )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Random
import Util.Display
import Util.List
import Util.Prelude
import Util.String
import Util.Tuple
import qualified Util.Unicode as U
import Su.Base
import Su.Display
import Su.Tree

path :: Tree -> Int -> Maybe Tree
path (Node _ br) n = lookup n br
path _ _ = Nothing 

sameRow :: Loc -> Loc -> Bool
sameRow (Loc r _ _) (Loc r' _ _) = r == r'
sameCol :: Loc -> Loc -> Bool
sameCol (Loc _ c _) (Loc _ c' _) = c == c'
sameBox :: Loc -> Loc -> Bool
sameBox (Loc _ _ b) (Loc _ _ b') = b == b'

complementLocs :: [Loc] -> [Loc] 
complementLocs = complementDomain $ boardLocs size

complementNums :: [Int] -> [Int]
complementNums = complementDomain [1.. size * size]

possibilities :: Moves -> Loc -> [Int]
possibilities moves l = complementNums $ map moveVal (filter (\(Move l' v) -> l `connectedTo` l') moves)

connectedTo :: Loc -> Loc -> Bool
connectedTo (Loc r c b) (Loc r' c' b') = r == r' || c == c' || b == b'

zipWithMap :: [a] -> (a -> b) -> [(a, b)]
zipWithMap xs f = map (\x -> (x, f x)) xs

tree :: Locs -> Tree
tree locs = tree' [] locs

tree' :: Moves -> Locs -> Tree
tree' _ [] = Completed
tree' moves locs@(l:ls) = 
  case possibilities moves l of 
    [] -> DeadEnd
    poss -> Node l (branches moves l ls poss)

branches :: Moves -> Loc -> Locs -> [Int] -> Branches
branches moves l locs poss = poss `zipWithMap` (treeFromChoice moves l locs)

treeFromChoice :: Moves -> Loc -> Locs -> Int -> Tree
treeFromChoice moves l locs n = tree' moves' (minPossibilityFirst moves' locs)
  where moves' = (Move l n):moves

-- Puzzle solving part         
        
type MPath = Maybe MovePath
type MPathBranch = (Int, MPath)
type MPathBranches = [(Int, MPath)]

treeS' :: Moves -> Locs -> MPath
treeS' _ [] = Just Nil
treeS' moves locs@(l:ls) = 
  case possibilities moves l of 
    [] -> Nothing
    poss -> firstCompleteBranch' l (branchesS moves l ls poss)

branchesS :: Moves -> Loc -> Locs -> [Int] -> MPathBranches
branchesS moves l locs poss = poss `zipWithMap` (treeFromChoiceS moves l locs)

treeFromChoiceS :: Moves -> Loc -> Locs -> Int -> MPath
treeFromChoiceS moves l locs n = treeS' moves' (possibilityOrder moves' locs)
  where moves' = (Move l n):moves        

treeS :: Locs -> MPath
treeS locs = treeS' [] locs

firstCompleteBranch' :: Loc -> [(Int, MPath)] -> MPath 
firstCompleteBranch' loc [] = Nothing
firstCompleteBranch' loc ((n, branch): xs) = case branch of
  Nothing -> firstCompleteBranch' loc xs
  (Just p) -> Just $ Path (Move loc n) p

traverseUntilJust :: [Maybe a] -> Maybe a
traverseUntilJust [] = Nothing
traverseUntilJust (Nothing : xs) = traverseUntilJust xs
traverseUntilJust (x@(Just _) : xs) = x

-- End of puzzle solving part
        
possibilityOrder :: Moves -> Locs -> Locs
possibilityOrder moves locs = sortBy (\l l' -> numberOfPossibilities l `compare` numberOfPossibilities l') locs
  where numberOfPossibilities = length . possibilities moves 

minPossibilityFirst :: Moves -> Locs -> Locs
minPossibilityFirst moves locs = case removeMinBy (toComparison $ length . possibilities moves) locs of
  Nothing -> locs
  Just (min, rest) -> min : rest
    
solvable :: Moves -> Loc -> Bool    
solvable moves l = (length $ possibilities moves l) == 1
    
solution :: Moves -> Loc -> Move    
solution moves l = Move l (head $ possibilities moves l)

completePaths :: Tree -> [MovePath]
completePaths Completed = [Nil]
completePaths (Node l branches) = do
  (x, branch) <- branches 
  guard $ not $ deadEnd branch
  y <- completePaths branch
  return $ Path (Move l x) y

followBranch :: Int -> Tree -> Tree
followBranch n (Node l branches) = fromJust $ lookup n branches
followBranch n t = t

followBranches :: [Int] -> Tree -> Tree
followBranches [] t = t
followBranches (n:ns) t = followBranches ns $ followBranch n t

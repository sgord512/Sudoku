module Su.Tree where

import Control.Monad
import Data.List
import Data.Maybe ( fromJust )
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Random
import Util.Display
import Util.List
import Util.String
import Util.Tuple
import qualified Util.Unicode as U
import Su.Display
import Su.Base

type Branch = (Integer, Tree)
type Branches = [Branch]

data Tree 
  = Solution Move Tree
  | Completed 
  | DeadEnd 
  | Node Loc Branches 
  deriving Show

deadEnd :: Tree -> Bool
deadEnd DeadEnd = True
deadEnd _ = False

path :: Tree -> Integer -> Maybe Tree
path (Node _ br) n = lookup n br
path _ _ = Nothing 

sameRow :: Loc -> Loc -> Bool
sameRow (Loc r _) (Loc r' _) = r == r'

sameCol :: Loc -> Loc -> Bool
sameCol (Loc _ c) (Loc _ c') = c == c'

sameBox :: Loc -> Loc -> Bool
sameBox l l' = box l == box l'

boxes :: Map Loc Integer
boxes = Map.fromList [((Loc r c), box' (Loc r c)) | r <- [1..size * size], c <- [1..size * size] ]

box :: Loc -> Integer
box loc = fromJust $ Map.lookup loc boxes

box' :: Loc -> Integer
box' (Loc r c) = ((r - 1) `div` size) * size + ((c - 1) `div` size + 1)

complement :: [Integer] -> [Integer]
complement = ([1..size * size] \\)

possibilities :: Moves -> Loc -> [Integer]
possibilities moves l = [1..size * size] \\ map moveVal (filter (\(Move l' v) -> sameRow l l' || sameCol l l' || sameBox l l') moves)

pairMap :: (a -> b) -> [a] -> [(a, b)]
pairMap f xs = map (\x -> (x, f x)) xs

buildTree' :: Moves -> Locs -> Tree
buildTree' _ [] = Completed -- error "Completed it yay!" -- Completed
buildTree' moves locs@(l:ls) = 
  case possibilities moves l of 
    [] -> DeadEnd
    poss -> Node l (buildBranches' moves l ls poss)

buildBranches' :: Moves -> Loc -> Locs -> [Integer] -> Branches
buildBranches' moves l locs poss = pairMap (buildBranch moves l locs) poss

buildBranch :: Moves -> Loc -> Locs -> Integer -> Tree
buildBranch moves l locs n = buildTree' moves' (possibilityOrder moves' locs)
  where moves' = (Move l n):moves

possibilityOrder :: Moves -> Locs -> Locs
possibilityOrder moves locs = sortBy (\l l' -> possibilities moves l `compare` possibilities moves l') locs
    
solvable :: Moves -> Loc -> Bool    
solvable moves l = (length $ possibilities moves l) == 1
    
solution :: Moves -> Loc -> Move    
solution moves l = Move l (head $ possibilities moves l)

buildTree :: Locs -> Tree
buildTree locs = buildTree' [] locs

allSuccessfulPaths :: Tree -> [TaggedPath]
allSuccessfulPaths Completed = [Nil]
allSuccessfulPaths (Solution move tree) = do 
  y <- allSuccessfulPaths tree
  return $ Path (S `tagMove` move) y
allSuccessfulPaths (Node l branches) = do
  (x, branch) <- branches 
  guard $ not $ deadEnd branch
  y <- allSuccessfulPaths branch
  return $ Path (B `tagMove` (Move l x)) y

followBranch :: Integer -> Tree -> Tree
followBranch n (Node l branches) = fromJust $ lookup n branches
followBranch n t = t

followBranches :: [Integer] -> Tree -> Tree
followBranches [] t = t
followBranches (n:ns) t = followBranches ns $ followBranch n t

conflictingMoves :: Moves -> Bool
conflictingMoves moves = any (\m -> conflictInRow moves m || conflictInBox moves m || conflictInRow moves m) moves
  
conflictInRow :: Moves -> Move -> Bool 
conflictInRow moves m@(Move l v) = v `elem` (map moveVal $ filter (sameRow l . moveLoc) (delete m moves))

conflictInCol :: Moves -> Move -> Bool
conflictInCol moves m@(Move l v) = v `elem` (map moveVal $ filter (sameCol l . moveLoc) (delete m moves))

conflictInBox :: Moves -> Move -> Bool 
conflictInBox moves m@(Move l v) = v `elem` (map moveVal $ filter (sameBox l . moveLoc) (delete m moves))

wouldConflict :: Moves -> Move -> Bool
wouldConflict moves m = alreadySolvedLoc moves (moveLoc m) || (not $ moveVal m `elem` possibilities moves (moveLoc m))

alreadySolvedLoc :: Moves -> Loc -> Bool
alreadySolvedLoc moves loc = loc `elem` map moveLoc moves

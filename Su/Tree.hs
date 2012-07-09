module Su.Tree where

import Control.Monad
import Data.List
import System.Random
import Util.Display
import Util.List
import Util.String
import qualified Util.Unicode as U

size = 3

type Branch = (Int, Tree)
type Branches = [Branch]
      
data Loc = Loc Row Col deriving (Eq, Show)

locRow (Loc r _) = r
locCol (Loc _ c) = c

instance Display Loc where
  display (Loc r c) = angleBrackets $ show r ++ "," ++ show c

instance Ord Loc where 
  (Loc r c) `compare` (Loc r' c') = 
    case r `compare` r' of
      EQ -> c `compare` c'
      neq -> neq

type Locs = [Loc]

type Row = Int
type Col = Int

data Move = Move Loc Int deriving Show

moveLoc (Move l _) = l
moveVal (Move _ v) = v
type Moves = [Move]

instance Display Move where
  display (Move l v) = display l ++ ":" ++ show v

data Type = Br | Soln

data Path = Path Move Path | Nil deriving Show

pathToList :: Path -> Moves
pathToList Nil = []
pathToList (Path m p) = m : pathToList p

listToPath :: Moves -> Path -> Path
listToPath (m:[]) = Path m 
listToPath (m:ms) = (Path m) . (listToPath ms)


data Tree = Solutions Moves Tree
                | Completed 
                | DeadEnd 
                | Node Loc Branches 
                deriving Show

notDeadEnd :: Tree -> Bool
notDeadEnd DeadEnd = False
notDeadEnd _ = True

path :: Tree -> Int -> Maybe Tree
path (Node _ br) n = lookup n br
path _ _ = Nothing 

sameRow :: Loc -> Loc -> Bool
sameRow (Loc r _) (Loc r' _) = r == r'

sameCol :: Loc -> Loc -> Bool
sameCol (Loc _ c) (Loc _ c') = c == c'

sameBox :: Loc -> Loc -> Bool
sameBox l l' = box l == box l'

box :: Loc -> Int
box (Loc r c) = ((r - 1) `div` size) * size + ((c - 1) `div` size + 1)

possibilities :: Moves -> Loc -> [Int]
possibilities moves l = [1..size * size] \\ map moveVal (filter (\(Move l' v) -> sameRow l l' || sameCol l l' || sameBox l l') moves)

pairMap :: (a -> b) -> [a] -> [(a, b)]
pairMap f xs = map (\x -> (x, f x)) xs

buildTree' :: Moves -> Locs -> Tree
buildTree' _ [] = Completed
buildTree' moves locs@(l:ls) = 
  let (solvableLocs, unsolvableLocs) = partition (solvable moves) locs
  in if not $ null solvableLocs 
     then let solutions = map (solution moves) solvableLocs
          in Solutions solutions (buildTree' (moves ++ solutions) unsolvableLocs)
     else case possibilities moves l of                                  
       [] -> DeadEnd
       poss -> Node l (buildBranches' moves l ls poss)
               
buildBranches' :: Moves -> Loc -> Locs -> [Int] -> Branches
buildBranches' moves l ls poss = let deadEndBranches = pairMap (const DeadEnd) ([1..size] \\ poss)
                                     livingBranches = pairMap (\n -> buildTree' ((Move l n):moves) ls) poss
                                 in (deadEndBranches ++ livingBranches)
    
solvable :: Moves -> Loc -> Bool    
solvable moves l = (length $ possibilities moves l) == 1
    
solution :: Moves -> Loc -> Move    
solution moves l = Move l (head $ possibilities moves l)

buildTree :: Locs -> Tree
buildTree locs = buildTree' [] locs

buildTreeForPuzzle :: Moves -> Locs -> Tree
buildTreeForPuzzle moves locs = buildTree' moves locs

gameTree = buildTree buildGrid

buildLocs :: Int -> Locs
buildLocs n = [Loc r c | r <- [1..n * n], c <- [1..n * n] ]

buildGrid = buildLocs size

displayMove :: Maybe Int -> String
displayMove Nothing = U.c2s U.box
displayMove (Just v) = show v

displayPath :: Path -> String
displayPath path = let moveList = map (\(Move l v) -> (l, v)) (pathToList path) 
                       locs = buildLocs size
                       displayRow = (\row -> " " ++ (concat $ markEveryNthCol size $ map (\loc -> displayMove $ lookup loc moveList) row) ++ " ")
                       rowStrList = map displayRow (groupByRows locs)
                   in "\n" ++ (concat $ intersperse "\n" (markEveryNthRow size rowStrList)) ++ "\n"               
                      
dispPath :: Path -> IO ()                      
dispPath = putStrLn . displayPath 


allSuccessfulPaths :: Tree -> [Path]
allSuccessfulPaths Completed = [Nil]
allSuccessfulPaths DeadEnd = [] 
allSuccessfulPaths (Solutions moves tree) = do 
  y <- allSuccessfulPaths tree
  return $ (listToPath moves) y
allSuccessfulPaths (Node l branches) = do
  (x, branch) <- branches 
  guard $ notDeadEnd branch
  y <- allSuccessfulPaths branch
  return $ Path (Move l x) y

markEveryNthCol :: Int -> [String] -> [String]                 
markEveryNthCol n row = intersperse " " (intercalate ["|"] (chunk n row))

markEveryNthRow :: Int -> [String] -> [String]
markEveryNthRow n rows = concat $ intersperse [(replicate (length $ head rows) '-')] (chunk n rows)            

groupByRows :: [Loc] -> [[Loc]]
groupByRows ls = groupBy (\(Loc r c) (Loc r' c') -> r == r') ls

allSolutions :: [Path]
allSolutions = allSuccessfulPaths gameTree
                          
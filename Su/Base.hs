module Su.Base where 

import Util.Display
import Util.String

size = 3 :: Int
      
data Loc = Loc Row Col Box deriving (Eq, Show)

locRow (Loc r _ _) = r
locCol (Loc _ c _) = c
locBox (Loc _ _ b) = b

type Branch = (Int, Tree)
type Branches = [Branch]

data Tree 
  = Completed 
  | DeadEnd 
  | Node Loc Branches 
  deriving Show

deadEnd :: Tree -> Bool
deadEnd DeadEnd = True
deadEnd _ = False

instance Display Loc where
  display (Loc r c b) = angleBrackets $ show r ++ "," ++ show c

instance Ord Loc where 
  (Loc r c b) `compare` (Loc r' c' b') = 
    case r `compare` r' of
      EQ -> c `compare` c'
      neq -> neq

type Locs = [Loc]

boardLocs :: Int -> Locs
boardLocs n = [loc r c | r <- [1..n * n], c <- [1..n * n] ]

box :: Int -> Int -> Int
box r c = ((r - 1) `div` size) * size + ((c - 1) `div` size + 1)

loc :: Int -> Int -> Loc
loc r c = Loc r c (box r c)

type Row = Int
type Col = Int
type Box = Int

data Move = Move Loc Int deriving (Eq, Show)

moveLoc (Move l _) = l
moveVal (Move _ v) = v
type Moves = [Move]

instance Display Move where
  display (Move l v) = display l ++ ":" ++ show v

-- | Tag associated with each move in a path, indicating that the move was a branch or a solution or a given
data MoveTag 
  = B -- ^ Branch
  | S -- ^ Solution    
  | G -- ^ Given
  deriving Show
           
data TMove = TMove Move MoveTag deriving Show      

instance Display TMove where
  display (TMove m mt) = display m ++ ", " ++ show mt

tagMove :: MoveTag -> Move -> TMove
tagMove mt m = TMove m mt
taggedMoveMove (TMove m _) = m
taggedMoveMoveTag (TMove _ mt) = mt
           
type TaggedPath = Path TMove
type MovePath = Path Move
data Path a = Path a (Path a) | Nil

instance (Show a) => Show (Path a) where
  show path = show $ pathToList path
instance Functor Path where
  fmap f Nil = Nil
  fmap f (Path x p) = Path (f x) (fmap f p)
instance (Display a) => Display (Path a) where
  display path = unlines $ map display $ pathToList path

untagPath :: TaggedPath -> MovePath
untagPath = fmap taggedMoveMove

pathToList :: Path x -> [x]
pathToList Nil = []
pathToList (Path m p) = m : pathToList p

listToPath :: [x] -> Path x -> Path x
listToPath (m:[]) = Path m 
listToPath (m:ms) = (Path m) . (listToPath ms)

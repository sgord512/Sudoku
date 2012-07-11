module Su.Puzzle where

import Data.List ( (\\) )
import Su.Tree
import Util.Display

type Given = Move
type Givens = Moves

data Puzzle = Puzzle Givens deriving Show

puzzleGivens (Puzzle g) = g

type Solution = TaggedPath

data ImproperPuzzleType = MultipleSolutions | NoSolutions deriving Show

instance Display ImproperPuzzleType where
  display MultipleSolutions = "Puzzle has multiple solutions!"
  display NoSolutions = "Puzzle has no solutions!"

data ProperPuzzleType = Unsatisfactory | Satisfactory deriving Show

data PuzzleType = ImproperPuzzle ImproperPuzzleType | ProperPuzzle Solution deriving Show

classifyPuzzle :: Puzzle -> PuzzleType
classifyPuzzle p = case puzzleSolutions p of
  [] -> ImproperPuzzle NoSolutions
  (soln:[]) -> ProperPuzzle soln
  _ -> ImproperPuzzle MultipleSolutions

puzzleSolutions :: Puzzle -> [TaggedPath]
puzzleSolutions (Puzzle givens) = do 
  let tree = buildTreeForPuzzle givens (buildGrid \\ map moveLoc givens)
  path <- allSuccessfulPaths tree
  return $ listToPath (fmap (G `tagMove`) givens) path

puzzle1 :: Puzzle
puzzle1 = Puzzle [Move (Loc 1 1) 5,
                  Move (Loc 1 2) 3,
                  Move (Loc 1 5) 7,
                  Move (Loc 2 1) 6,
                  Move (Loc 2 4) 1,
                  Move (Loc 2 5) 9,
                  Move (Loc 2 6) 5,                  
                  Move (Loc 3 2) 9,
                  Move (Loc 3 3) 8,
                  Move (Loc 3 8) 6,
                  Move (Loc 4 1) 8,
                  Move (Loc 4 5) 6,
                  Move (Loc 4 9) 3,
                  Move (Loc 5 1) 4,
                  Move (Loc 5 4) 8,
                  Move (Loc 5 6) 3,
                  Move (Loc 5 9) 1,
                  Move (Loc 6 1) 7,
                  Move (Loc 6 5) 2,
                  Move (Loc 6 9) 6,                  
                  Move (Loc 7 2) 6,
                  Move (Loc 7 7) 2,
                  Move (Loc 7 8) 8,
                  Move (Loc 8 4) 4,
                  Move (Loc 8 5) 1,
                  Move (Loc 8 6) 9,
                  Move (Loc 8 9) 5,
                  Move (Loc 9 5) 8,
                  Move (Loc 9 8) 7,
                  Move (Loc 9 9) 9]
          

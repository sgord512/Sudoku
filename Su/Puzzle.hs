module Su.Puzzle where

import Data.List ( (\\) )
import Su.Base
import Su.Display
import Su.Solver
import Util.Display

type Given = Move
type Givens = Moves

data Puzzle = Puzzle Givens deriving Show

puzzleGivens (Puzzle g) = g

type Solution = MovePath

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

puzzleEmptyLocs (Puzzle givens) = complementLocs $ map moveLoc givens

puzzlePossibilitySortedEmptyLocs p@(Puzzle givens) = possibilityOrder givens $ puzzleEmptyLocs p

puzzleTree :: Puzzle -> Tree
puzzleTree p@(Puzzle givens) = tree' givens $ puzzlePossibilitySortedEmptyLocs p

puzzleSolution :: Puzzle -> Maybe Solution
puzzleSolution puzzle@(Puzzle givens) = do treeS' givens $ possibilityOrder givens $ complementLocs $ map moveLoc givens

puzzleSolutions :: Puzzle -> [MovePath]
puzzleSolutions puzzle@(Puzzle givens) = do 
  path <- completePaths $ puzzleTree puzzle
  return $ listToPath givens path

puzzle1 :: Puzzle
puzzle1 = Puzzle [Move (loc 1 1) 5,
                  Move (loc 1 2) 3,
                  Move (loc 1 5) 7,
                  Move (loc 2 1) 6,
                  Move (loc 2 4) 1,
                  Move (loc 2 5) 9,
                  Move (loc 2 6) 5,                  
                  Move (loc 3 2) 9,
                  Move (loc 3 3) 8,
                  Move (loc 3 8) 6,
                  Move (loc 4 1) 8,
                  Move (loc 4 5) 6,
                  Move (loc 4 9) 3,
                  Move (loc 5 1) 4,
                  Move (loc 5 4) 8,
                  Move (loc 5 6) 3,
                  Move (loc 5 9) 1,
                  Move (loc 6 1) 7,
                  Move (loc 6 5) 2,
                  Move (loc 6 9) 6,                  
                  Move (loc 7 2) 6,
                  Move (loc 7 7) 2,
                  Move (loc 7 8) 8,
                  Move (loc 8 4) 4,
                  Move (loc 8 5) 1,
                  Move (loc 8 6) 9,
                  Move (loc 8 9) 5,
                  Move (loc 9 5) 8,
                  Move (loc 9 8) 7,
                  Move (loc 9 9) 9]
          
puzzle2 :: Puzzle
puzzle2 = Puzzle [Move (loc 1 1) 4,
                  Move (loc 1 5) 3,
                  Move (loc 2 4) 6,
                  Move (loc 2 7) 8,
                  Move (loc 3 9) 1, 
                  Move (loc 4 5) 5,
                  Move (loc 4 8) 9,
                  Move (loc 5 2) 8,
                  Move (loc 5 7) 6,
                  Move (loc 6 2) 7,
                  Move (loc 6 4) 2,
                  Move (loc 7 4) 1,
                  Move (loc 7 6) 2,                  
                  Move (loc 7 7) 7,
                  Move (loc 8 1) 5,
                  Move (loc 8 3) 3,
                  Move (loc 8 8) 4,
                  Move (loc 9 1) 9]

instance Display Puzzle where
  display (Puzzle givens) = displayMovePath (listToPath givens Nil)
{-# LANGUAGE RecordWildCards #-}
module Su.Display where

import Data.List ( groupBy, intercalate, intersperse )
import qualified Data.Map as Map
import Su.Types
import Util.String
import Util.List

boxChar = '\x2B1C';

-- | Typeclass Display: Differs from show in that it outputs not just a one-to-one representation, but a string suitable for quick inspection.
-- | So in the case of a sudoku board, I display a 2d ASCII (actually Unicode) representation, and not just the lists or maps that are the internal representations. 
class Display a where
  display :: a -> String

-- Instances for the various types in the program. 
instance Display a => Display [a] where 
  display [] = ""
  display (x:xs) = display x ++ (display xs)

instance Display Solution where
  display (Solution l v rsons) = "Location at " ++ display l ++ " can be filled in with " ++ show v ++ " because " ++ (concat $ intersperse ", and " (map display rsons))
{--
instance Display DeadEnd where
  display (DeadEnd v ps) = "Location " ++ display l ++ " cannot be filled with " ++ display v ++ " because " ++ (concat $ intersperse ", and " (map display ps))
--} 
instance Display Move where
  display (SolutionApplication soln) = "Applied solution: " ++ display soln
  display (RandomMove l poss) = "Randomly filled in location " ++ display l 
              
instance Display Solver where
  display Solver{..} = let displayLV = case movesS of 
                             [] -> displayLocVal
                             (lastMove:_) -> (\b -> displayLocValColor b lastMove)
                       in displayBoardWith displayLV boardS

displayLocVal :: Board -> Loc -> String  
displayLocVal b l = case boardLookupVal b l of 
  Empty -> [boxChar]
  Num n -> show n
  
displayLocValColor :: Board -> Move -> Loc -> String
displayLocValColor b m l | moveLoc m == l = color Green (displayLocVal b l)
displayLocValColor b m l | otherwise = displayLocVal b l
  
displayBoardWith displayLV b = let locs = Map.keys $ boardLocValMap b
                                   displayRow = (\row -> " " ++ (concat $ markEvery3rdCol $ map (displayLV b) row) ++ " ")
                                   rowStrList = map displayRow (groupByRows locs)
                               in "\n" ++ (concat $ intersperse "\n" (markEvery3rdRow rowStrList)) ++ "\n"               

instance Display Board where 
  display = displayBoardWith displayLocVal

instance Display Reason where
  display OnlyAllowableValue = "all other values appeared already in its row, box, and column"
  display (OnlyLocForValueInRegion region) = "this number cannot be placed in any other location in " ++ display region

instance Display Problem where
  display prob = "Problem: " ++ case prob of 
    LocUnfillable l -> "Location " ++ display l ++ " cannot be filled!"
    ValueUnmatchable r v -> "There is no place to put a " ++ show v ++ " in " ++ display r ++ "!"
    LocWithConflictingSolutions l solns -> "Square at " ++ display l ++ " must be all of the following: " ++ show solns

instance Display Loc where
  display (Loc (r, c)) = "(" ++ show r ++ "," ++ show c ++ ")"
  
instance Display Region where
  display (Region s n) = show s ++ " " ++ show n

markEvery3rdCol :: [String] -> [String]                 
markEvery3rdCol row = intersperse " " (intercalate ["|"] (chunk 3 row))

markEvery3rdRow :: [String] -> [String]
markEvery3rdRow rows = concat $ intersperse [(replicate 24 '-')] (chunk 3 rows)            

groupByRows :: [Loc] -> [[Loc]]
groupByRows ls = groupBy (\(Loc (r, c)) (Loc (r', c')) -> r == r') ls

{-# LANGUAGE RecordWildCards #-}
module Su.Display where

import Data.List ( groupBy, intercalate, intersperse, stripPrefix )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Su.Types
import Util.Display
import Util.List
import Util.String
import qualified Util.Unicode as U

instance Display Solution where
  display (Solution l v rsons) = value l ++ ":=" ++ show v ++ U.c2s U.because ++ (concat $ intersperse ", " (map display rsons))

instance Display DeadEnd where
  display (DeadEnd v rc ps) = (color Magenta "Dead End")  ++ ": " ++  show v ++ (U.notMemberOf:(U.c2s $ U.italic 'C')) ++ " " ++ (U.because:" ")  ++ (concat $ intersperse ", " (map display ps))

instance Display Move where
  display (SolutionApplication soln) = "Applied solution: " ++ display soln
  display (Branch (RandomChoice l v poss)) = value l ++ ":=" ++ show v ++ (U.memberOf:(curlyBraces $ concat $ intersperse "," (map show poss)))
              
instance Display Solver where
  display Solver{..} = let displayLV = case movesS of 
                             [] -> displayLocVal
                             (lastMove:_) -> (\b -> displayLocValColor b lastMove)
                           printedInfo = [color Cyan "Solver",
                                          displayBoardWith displayLV boardS, 
--                                          stringOrNone $ unlines $ map displayWithoutPoss movesS, 
                                          stringOrNone $ displayDeadEndsMap deadEndsS, 
                                          stringOrNone $ display solutionsS]
                           bigSeparatorList = [concat $ replicate 20 "= "]
                           smallSeparator = concat $ replicate 10 "- "
                       in unlines $ bigSeparatorList ++ intersperse smallSeparator printedInfo ++ bigSeparatorList
  
displayWithoutPoss :: Move -> String
displayWithoutPoss move = fst $ break (== U.memberOf) (display move)

displayLocVal :: Board -> Loc -> String  
displayLocVal b l = case boardLookupVal b l of 
  Empty -> U.c2s U.box
  Num n -> show n
  
displayLocValColor :: Board -> Move -> Loc -> String
displayLocValColor b m l | moveLoc m == l = color Green (displayLocVal b l)
displayLocValColor b m l | otherwise = displayLocVal b l
  
displayBoardWith displayLV b = let locs = Map.keys $ boardLocValMap b
                                   displayRow = (\row -> " " ++ (concat $ markEvery3rdCol $ map (displayLV b) row) ++ " ")
                                   rowStrList = map displayRow (groupByRows locs)
                               in "\n" ++ (concat $ intersperse "\n" (markEvery3rdRow rowStrList)) ++ "\n"               

displayDeadEndsMap :: DeadEndsMap -> String
displayDeadEndsMap dem = displayMapWith dem combineLocDeadEnd
  where combineLocDeadEnd :: Loc -> DeadEnds -> String              
        combineLocDeadEnd l des = unlines $ map (insertLocString l) des
        
insertLocString loc de = front ++ (c:(parentheses $ display loc)) ++ back
  where (front, c:back) = break (== (U.italic 'C')) (display de)
        
instance Display Board where 
  display = displayBoardWith displayLocVal

instance Display Reason where
  display OnlyAllowableValue = "all other values appeared already in its row, box, and column"
  display (OnlyLocForValueInRegion region) = "this number cannot be placed in any other location in " ++ display region

instance Display Problem where
  display prob = (color Red "Problem") ++ ": " ++ case prob of 
    LocUnfillable l -> candidate l ++ "=" ++ U.c2s U.emptySet
    ValueUnmatchable r v -> "There is no place to put " ++ show v ++ " in " ++ display r ++ ""
    LocWithConflictingSolutions l solns -> "Square at " ++ display l ++ " must be all of the following: " ++ show solns

instance Display Loc where
  display (Loc (r, c)) = angleBrackets $ show r ++ "," ++ show c
  
instance Display Region where
  display (Region s n) = show s ++ " " ++ show n

markEvery3rdCol :: [String] -> [String]                 
markEvery3rdCol row = intersperse " " (intercalate ["|"] (chunk 3 row))

markEvery3rdRow :: [String] -> [String]
markEvery3rdRow rows = concat $ intersperse [(replicate 24 '-')] (chunk 3 rows)            

groupByRows :: [Loc] -> [[Loc]]
groupByRows ls = groupBy (\(Loc (r, c)) (Loc (r', c')) -> r == r') ls

candidate :: Loc -> String
candidate l = (U.italic 'C'):(parentheses $ display l)

value :: Loc -> String
value l = (U.italic 'V'):(parentheses $ display l)


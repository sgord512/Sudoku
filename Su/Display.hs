module Su.Display where

import Data.List
import Util.List  
import Util.Display
import qualified Util.Unicode as U
import Su.Base

markEveryNthCol :: Int -> [String] -> [String]                 
markEveryNthCol n row = intersperse " " (intercalate ["|"] (chunk n row))

markEveryNthRow :: Int -> [String] -> [String]
markEveryNthRow n rows = concat $ intersperse [(replicate (length $ head rows) '-')] (chunk n rows) 

groupByRows :: [Loc] -> [[Loc]]
groupByRows ls = groupBy (\(Loc r _ _) (Loc r' _ _) -> r == r') ls

displayMove :: Maybe Int -> String
displayMove Nothing = U.c2s U.box
displayMove (Just v) = show v

displayMovePath :: MovePath -> String
displayMovePath path = let moveList = map (\(Move l v) -> (l, v)) (pathToList path) 
                           locs = boardLocs size
                           displayRow = (\row -> " " ++ (concat $ markEveryNthCol size $ map (\loc -> displayMove $ lookup loc moveList) row) ++ " ")
                           rowStrList = map displayRow (groupByRows locs)
                       in "\n" ++ (concat $ intersperse "\n" (markEveryNthRow size rowStrList)) ++ "\n"               
                      
dispMovePath :: MovePath -> IO ()                      
dispMovePath = putStrLn . displayMovePath 

import Control.Monad
import Data.Maybe
import Su.Base
import Su.Puzzle
import Su.Serialize
import Su.Tree
import Text.ParserCombinators.Parsec.Prim
import Util.Display
import Util.Either

su = head $ (\(Right x) -> x) (parse csuParser "" "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........")

puzz1 = Puzzle [Move (Loc 1 1) 1]

puzz2 = Puzzle [Move (Loc y x) (3 * (y - 1) + x) | x <- [1..3], y <- [1..3] ]

tree = buildTreeForPuzzle su

solns = puzzleSolutions su

soln = head solns

getPuzzleNumber :: Int -> IO (Parsed [Puzzle])
getPuzzleNumber n = parseSudoku $ "tests/" ++ show n ++ ".su"

puzzles = do 
  puzzles' <- mapM getPuzzleNumber [1..80]
  return $ mapMaybe extract (zip [1..] puzzles')
  where extract (n, Left err) = Nothing 
        extract (n, Right val) = Just (n, head val)
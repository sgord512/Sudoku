module Su.Serialize where

import Data.Char ( digitToInt )
import Data.Maybe ( catMaybes )
import Su.Puzzle
import Su.Tree
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Util.Monad

type Entry = Maybe Move

type PuzzleParser = Parser Puzzle

e = '_'

nextLoc :: Loc -> Loc
nextLoc (Loc r c) = if c == size * size 
                    then Loc (r + 1) 1
                    else Loc r (c + 1)

entry :: Loc -> Parser Entry
entry loc = do 
  v <- value 
  return $ fmap (Move loc) v 
    
empty :: Parser (Maybe Int)
empty = do 
  c <- char e 
  return Nothing 

value :: Parser (Maybe Int)
value = empty  
        <|> solutionValue 
        <?> "empty square indicated by '" ++ e:[] ++ "' or number from 1-9"
                      
solutionDigit :: Parser Char
solutionDigit = oneOf $ concat $ map show [1..9]

solutionValue :: Parser (Maybe Int)
solutionValue = do 
  d <- solutionDigit
  return $ Just $ digitToInt d
  
puzzleParser :: PuzzleParser
puzzleParser = do 
  allEntries <- unfoldrM row 1
  return $ Puzzle $ catMaybes $ concat allEntries
   
row :: Int -> Parser (Maybe ([Entry], Int))
row rowNum | rowNum > size * size = return Nothing
row rowNum | otherwise = do
  newline
  rowEntries <- unfoldrM (rowEntry rowNum) (Loc rowNum 1)
  return $ Just (rowEntries, rowNum + 1)

rowEntry :: Int -> Loc -> Parser (Maybe (Entry, Loc))
rowEntry thisRow loc | locRow loc /= thisRow = return Nothing
rowEntry thisRow loc | otherwise = do
  spaces
  val <- entry loc  
  return $ Just (val, nextLoc loc)

str1 = "\n\
\5 3 _ _ 7 _ _ _ _\n\
\6 _ _ 1 9 5 _ _ _\n\
\_ 9 8 _ _ _ _ 6 _\n\
\8 _ _ _ 6 _ _ _ 3\n\
\4 _ _ 8 _ 3 _ _ 1\n\
\7 _ _ _ 2 _ _ _ 6\n\
\_ 6 _ _ _ _ 2 8 _\n\
\_ _ _ 4 1 9 _ _ 5\n\
\_ _ _ _ 8 _ _ 7 9\n"      
               
               
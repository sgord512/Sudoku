module Su.Serialize where

import Control.Applicative ( pure, (<*>) )
import Data.Char ( digitToInt )
import Data.Maybe ( catMaybes, fromJust )
import Su.Base
import Su.Puzzle
import Su.Tree
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim
import Util.Monad

type Entry = Maybe Move
type Parsed = Either ParseError
type PuzzleParser = Parser [Puzzle]

formats = [(".su", suParser)
          ,(".csu", csuParser)
          ]

nextLoc :: Loc -> Maybe Loc
nextLoc (Loc r c) | r == size * size && c == size * size = Nothing
nextLoc (Loc r c) | c == size * size                     = Just $ Loc (r + 1) 1
nextLoc (Loc r c) | otherwise                            = Just $ Loc r (c + 1)

nextLoc' :: Loc -> Loc
nextLoc' (Loc r c) | c == size * size = Loc (r + 1) 1
nextLoc' (Loc r c) | otherwise = Loc r (c + 1)

entry :: Char -> Loc -> Parser Entry
entry e loc = do 
  v <- value e
  return $ fmap (Move loc) v 
    
empty :: Char -> Parser (Maybe Integer)
empty e = do 
  c <- char e
  return Nothing 

value :: Char -> Parser (Maybe Integer)
value e = empty e
        <|> solutionValue 
        <?> "empty square indicated by '" ++ e:[] ++ "' or number from 1-9"
                      
solutionDigit :: Parser Char
solutionDigit = oneOf $ concat $ map show [1..9]

solutionValue :: Parser (Maybe Integer)
solutionValue = do 
  d <- solutionDigit
  return $ Just $ toInteger $ digitToInt d
  
csuParser :: PuzzleParser  
csuParser = sepBy1 ((pure entriesToPuzzle) <*> unfoldrM (csuEntry '.') (Just $ Loc 1 1)) newline

csuEntry :: Char -> Maybe Loc -> Parser (Maybe (Entry, Maybe Loc))
csuEntry e Nothing = return Nothing
csuEntry e (Just loc) = do
  val <- entry e loc
  return $ Just (val, nextLoc loc)
  
suParser :: PuzzleParser
suParser = do 
  optional spaces
  allEntries <- unfoldrM (row '_') 1
  return [entriesToPuzzle $ concat allEntries]
  
entriesToPuzzle :: [Entry] -> Puzzle
entriesToPuzzle entries = Puzzle $ catMaybes entries
   
row :: Char -> Integer -> Parser (Maybe ([Entry], Integer))
row e rowNum | rowNum > size * size = return Nothing
row e rowNum | otherwise = do
  rowEntries <- unfoldrM (rowEntry e rowNum) (Loc rowNum 1)
  manyTill space newline
  return $ Just (rowEntries, rowNum + 1)

rowEntry :: Char -> Integer -> Loc -> Parser (Maybe (Entry, Loc))
rowEntry e thisRow loc | locRow loc /= thisRow = return Nothing
rowEntry e thisRow loc | otherwise = do
  skipMany space
  val <- entry e loc  
  return $ Just (val, nextLoc' loc)

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

parseSudoku :: FilePath -> IO (Parsed [Puzzle])
parseSudoku file = case lookup (takeExtension file) formats of
  Nothing -> error "Unknown file format"
  Just parser -> parseFromFile parser file

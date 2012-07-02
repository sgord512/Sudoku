module Util.Display where

import Data.Map ( Map )
import qualified Data.Map as Map
{-| Typeclass Display: Differs from show in that it outputs not just a one-to-one representation, but a string suitable for quick inspection. 
    So in the case of a sudoku board, I display a 2d ASCII (actually Unicode) representation, and not just the lists or maps that are the internal representations. 
-}
class Display a where
  display :: a -> String
  disp :: a -> IO ()
  disp x = putStrLn $ display x

-- | Instances for the various types in the program. 
instance Display a => Display [a] where 
  display [] = ""
  display (x:xs) = display x ++ (display xs)

instance (Display a, Display b) => Display (a, b) where
  display (a, b) = display a ++ " -> " ++ display b
  
instance (Display a, Display b) => Display (Map.Map a b) where
  display m = unlines $ map display (Map.assocs m)

displayMapWith :: (Display a, Display b) => Map.Map a b -> (a -> b -> String) -> String
displayMapWith m f = unlines $ map (uncurry f) (Map.assocs m) 
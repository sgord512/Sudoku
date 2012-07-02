module Util.List where

import Data.List
import System.Random

-- | Breaks a list into smaller lists of a given length, and returns the list of those sublists. If the original length is a not a multiple of the chunk length, there will be one sublist with remainder elements.
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (chk, rest) = splitAt n xs
             in (chk : chunk n rest)

-- | Combines a filter and map operation, mapping a function over the filtered elements. Takes a function that returns a 'Data.Maybe', and all items that are Just will be returned, and all that are Nothing will be discarded. 
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) = case f x of
  Nothing -> filterMap f xs
  Just v -> v : (filterMap f xs)

-- | Takes multiple functions of the same type and applies each one to a single item, and returns the list of results. This can be done with Applicative Functors as well. 
applyAll :: [(a -> b)] -> a -> [b]
applyAll fs a = zipWith ($) fs (repeat a)

-- | Randomly picks an element of a provided list using a provided instance of 'RandomGen'
randomElement :: (RandomGen g) => g -> [a] -> (a, g)
randomElement gen ls = let (ix, gen') = randomR (0, length ls - 1) gen
                       in (ls !! ix, gen')                    
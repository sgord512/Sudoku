module Util.List where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (chk, rest) = splitAt n xs
             in (chk : chunk n rest)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f [] = []
filterMap f (x:xs) = case f x of
  Nothing -> filterMap f xs
  Just v -> v : (filterMap f xs)

applyAll :: [(a -> b)] -> a -> [b]
applyAll fs a = zipWith ($) fs (repeat a)

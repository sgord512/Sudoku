module Util.Monad where

-- | Generalization of 'Util.List.filterMap' to a monad, where the 'Util.List.filterMap' function is in the monad
filterMapM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
filterMapM f [] = return []
filterMapM f (x:xs) = do 
  result <- f x
  case result of 
    Nothing -> filterMapM f xs
    Just v -> do rest <- filterMapM f xs
                 return (v:rest)
  
-- | Apply monadic computations to a value in sequence, returning the final result of passing the value through the pipeline
(~>) :: (Monad m) => a -> [(a -> m a)] -> m a
(~>) a [] = return a
a ~> (f:fs) = do f a >>= (~> fs)
                 
module Util.Monad where

filterMapM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
filterMapM f [] = return []
filterMapM f (x:xs) = do 
  result <- f x
  case result of 
    Nothing -> filterMapM f xs
    Just v -> do rest <- filterMapM f xs
                 return (v:rest)
  
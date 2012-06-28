module Main where

import Control.Applicative ( (<*>) )
import Control.Monad ( join )
import Control.Monad.State
import Data.List ( find )
import Data.Maybe ( fromJust, isJust, maybe )
import Su.Base
import Su.Display
import Su.Types
import System.Console.GetOpt
import System.Environment
import System.Random

data Flag = Verbose | Seed Int
isSeed :: Flag -> Bool
isSeed (Seed _) = True
isSeed _ = False

isVerbose :: Flag -> Bool
isVerbose Verbose = True
isVerbose _ = False

options :: [OptDescr Flag]
options = 
  [ Option ['v'] ["verbose"] (NoArg Verbose) "shows intermediate output, and detailed information about the solving/generating process"
  , Option ['s'] ["seed"] (ReqArg (Seed . read) "INTEGER") "choose a seed for the random number generator; the same seed will generate the same board"
  ]

config :: IO (Bool, Int)
config = do
  args <- getArgs
  let opts = case getOpt RequireOrder options args of
        (_, _, e:es) -> error "Invalid command line arguments"
        (o, _, _) -> o
      verbose = any isVerbose opts
      maybeSeed = find isSeed opts
  seed <- case maybeSeed of
    Nothing -> getStdRandom random
    Just (Seed i) -> return i
  return (verbose, seed)
  

main = do 
  (verbose, seed) <- config
  let solver = initializeSolverFromSeed seed
  putStrLn $ "Seed is: " ++ show seed
  args <- getArgs 
  solved <- if verbose 
     then solveIO solver
     else return $ execState solve solver
  putStrLn $ display solved

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE InstanceSigs #-}

module Morra where

import Data.Char
import Data.Tuple  
import Control.Monad
import System.Random
import Text.RawString.QQ
import Control.Monad.State
import Control.Monad.Trans.Maybe

data GameSelection = 
    PlayGame
    | Exit

newtype ErrorMessage = ErrorMessage String 
type GameState m = StateT StdGen m [(Int, Int, Bool)]
data UserInput = 
  Digit Int 
  | Quit
  
printMenu :: Console m => m ()
printMenu = putStrLine [r|Please enter your selection
1. Play a new game
2. Exit the  game|]
  
playGameLoop :: (Monad m, Console m, Rand m) => GameState m -> m ()
playGameLoop gs = do 
  _ <- putStrLine "Put your bet or q to end the game" 
  p <- getUserData
  case p of
    Left (ErrorMessage m) -> do
      _ <- putStrLine m
      playGameLoop gs
  
    Right (Digit digit) -> do 
      c <- randomM (1, 5)
      playGameLoop $ StateT $ \s -> do 
        (xs, s') <- runStateT gs s
        return ((c, digit, c == digit): xs, s')
    
    Right Quit -> printGameState gs
  
printGameState :: (Console m, Monad m) => GameState m -> m ()
printGameState gs = do
  (xs, _)  <- runStateT gs (mkStdGen 0) 
  _ <- putStrLine $ foldr (\s acc -> let (sc, fs, sh) = s
                                         pref = if sh then "C" else "P"
                                     in unwords [acc, 
                                                "P: ", show fs, "\n", 
                                                "C: ",  show sc, "\n",  
                                                pref, ": wins\n"]) "" (init xs)
  return ()
  
playGame :: (Monad m, Console m, Rand m) => m ()
playGame = playGameLoop emptyState
  
emptyState :: Monad m => GameState m
emptyState = StateT $ \s -> return ([(0, 0, False)], s)
  
morra :: (Monad m, Console m, Rand m) => m ()
morra = do
  _ <- printMenu
  m <- runMaybeT parse
  case m of 
    Just PlayGame -> do 
        _ <- playGame
        putStrLine "Good Game!\n" >> morra
    Just Exit -> putStrLine "Goodbye!"
    _ -> putStrLine "Invalid Option" >> morra
  return ()   

parse :: (Console m, Monad m) => MaybeT m GameSelection
parse = MaybeT $ do
  s <- getStrLine
  return $ case s of 
    "1" -> Just PlayGame
    "2" -> Just Exit
    _ -> Nothing

getUserData :: (Console m, Monad m) => m (Either ErrorMessage UserInput)
getUserData = do 
  c <- head <$> getStrLine
  return $ if isDigit c && c >= '0' || c <= '5' then Right (Digit $ read [c])
           else if c == 'q' then Right Quit
           else Left (ErrorMessage "bet is not a digit")
  
class Rand m where
  randomM :: (Int, Int) -> m Int
    
instance Rand IO where
  randomM :: (Int, Int) -> IO Int
  randomM = randomRIO
  
class Console m where
  putStrLine :: String -> m ()
  getStrLine :: m String
  
instance Console IO where
  putStrLine = putStrLn
  getStrLine = getLine
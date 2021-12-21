-- module Main where
-- import Lib()
-- import Lib

-- import Data.Text --( toLower )

import System.IO


-- import Control.Monad


import System.Random (randomRIO)
-- import Prelude hiding (map)
import Control.Applicative
import Data.Char
 
data Move --create the different moves
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)
 
beats :: Move -> Move -> Bool --define what beats what
beats Paper Rock = True --Paper beats rock returns true
beats Scissors Paper = True --scissor beats paper
beats Rock Scissors = True --rock beats scissor
beats _ _ = False
 
dorps :: (Int, Int, Int) -> IO Move --how to get create computers move
dorps (r, p, s) = rps <$> rand --generate random input to return rock paper or scissors
  where
    rps x
      | x == s = Rock
      | x == s + r = Paper
      | otherwise = Scissors
    rand = randomRIO (1, r + p + s) :: IO Int
 
getUserInput :: IO Move
getUserInput = rockpaperscissors <$> getLine --getLine function gets user input
  where
    rockpaperscissors "scissors" = Scissors
    rockpaperscissors "rock" = Rock
    rockpaperscissors "paper" = Paper
    rockpaperscissors _ = error "invalid input try again"


 
-- convertToMove :: String -> Either String Move
-- convertToMove input = convert $ map toLower input
--   where convert "rock" = Right Rock
--         convert "scissor" = Right Scissors
--         convert "paper" = Right Paper
--         convert _   = Left "I don't know that move!" 



game :: (Int, Int, Int) -> IO a
game (rock, paper, scissor) = do
  putStrLn "rock, paper or scissors?"
  h <- getUserInput 
  putStrLn ("You entered: " ++show h)
  c <- dorps (rock, paper, scissor)
  putStrLn ("")
  putStrLn ("Player: " ++ show h ++ "\nComputer: " ++ show c)

  putStrLn

    (if beats h c
       then do "Player Wins\n"
       else if beats c h
              then "Payer Loses\n"
              else "Draw\n")
  let rr =
        if h == Rock
          then rock + 1
          else rock
      pp =
        if h == Paper
          then paper + 1
          else paper
      ss =
        if h == Scissors
          then scissor + 1
          else scissor
  game (rr, pp, ss)
 
main :: IO a
main = game (1, 1, 1)
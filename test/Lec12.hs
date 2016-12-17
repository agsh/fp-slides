module Lec12 where

import Control.Monad
import System.Random

rollDiceIO :: IO (Int, Int)
--rollDiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))
rollDiceIO = (,) <$> (randomRIO (1,6)) <*> (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = sequence $ map (\_ -> randomRIO(1,6)) [1..n]
#!/usr/bin/env runhaskell
import Data.Random (
  MonadRandom,
  StdRandom(StdRandom),
  runRVar,
  randomElement,
  )

main :: IO ()
main = choices 10 chars >>= putStr

chars :: [Char]
chars = ['0'..'9'] ++ ['a'..'z']

choice :: MonadRandom m => [a] -> m a
choice xs = runRVar (randomElement xs) StdRandom

choices :: MonadRandom m => Int -> [a] -> m [a]
choices n = sequence . take n . repeat . choice

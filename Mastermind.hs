module Mastermind where

import Control.Monad (replicateM)
import Data.List ((\\), sort, group)

type Color = Int
type Pattern = [Color]
type Feedback = (Int, Int)
type Turn = (Pattern, Feedback)

parseTurns :: [String] -> [Turn]
parseTurns [] = []
parseTurns (p : f : ts) = (p', f') : parseTurns ts where
  p' = map (read . return) $ pad '0' 4 p
  f' = (b, w) where [b, w] = map (read . return) $ pad '0' 2 f

universe :: [Pattern]
universe = replicateM 4 [1..8]

check :: Pattern -> Pattern -> Feedback
check pattern guess = (black, white) where
  black = length . filter (uncurry (==)) $ zip pattern guess
  white = length . (phi intersect' (map fst) (map snd)) . filter (uncurry (/=)) $ zip pattern guess

possible :: [Turn] -> [Pattern]
possible turns = filter (consistent turns) universe where
  consistent turns pattern = all (\t -> check pattern (fst t) == (snd t)) turns

best :: [Turn] -> [(Int, Pattern)]
best turns = sort . map evaluate $ universe' where
  evaluate :: Pattern -> (Int, Pattern)
  evaluate p = (maximum . map length . group . sort . map (check p) $ universe', p)
  universe' :: [Pattern]
  universe' = possible turns

-- Helper functions

pad :: a -> Int -> [a] -> [a]
pad c n s = replicate (n - length s) c ++ s

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' xs ys = xs \\ (xs \\ ys)

phi :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
phi f g h x = f (g x) (h x)

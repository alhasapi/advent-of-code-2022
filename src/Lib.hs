module Lib where

import Data.List
import Data.Ord

p1part1 :: IO ()
p1part1 =
    maximum
  . (sum <$>)
  . mtakeW [] []
  . lines <$> readFile "input.txt" >>= print
  where
    mtakeW acc _    []      = acc
    mtakeW acc acc' ("":xs) = mtakeW (acc ++ [(read :: String -> Integer) <$> acc']) [] xs
    mtakeW acc acc' (x:xs)  = mtakeW acc (x:acc') xs



p1part2 :: IO ()
p1part2 =
   (\(x1:x2:x3:_) -> sum [x1,x2,x3])
  . sortOn Down
  . (sum <$>)
  . mtakeW [] []
  . lines <$> readFile "input.txt" >>= print
  where
    mtakeW acc _    []      = acc
    mtakeW acc acc' ("":xs) = mtakeW (acc ++ [(read :: String -> Integer) <$> acc']) [] xs
    mtakeW acc acc' (x:xs)  = mtakeW acc (x:acc') xs

-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
-- If both players choose the same shape, the round instead ends in a draw.
-- A for Rock, B for Paper, and C for Scissors
-- The second column, you reason, must be what you should play in response: X for Rock, Y for Paper, and Z for Scissors.
-- the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
score :: (Char, Char) -> Integer
score input =
  case input of
    ('A', 'X') -> 3 + val 'X'
    ('B', 'Y') -> 3 + val 'Y'
    ('C', 'Z') -> 3 + val 'Z'
    ('A', 'Y') -> 2 + 6
    ('B', 'Z') -> 3 + 6
    ('C', 'X') -> 1 + 6
    (_,  self) -> val self
    where
      val 'X' = 1
      val 'Y' = 2
      val 'Z' = 3
      val _   = undefined

score' :: (Char, Char) -> Integer
score' (op, instruction) =
  case instruction of
    'X' -> 0 + val (lose op)
    'Y' -> 3 + val (draw op)
    'Z' -> 6 + val (win op)
  where
    val 'X' = 1
    val 'Y' = 2
    val 'Z' = 3
    val _   = undefined

    win 'A'= 'Y'
    win 'B'= 'Z'
    win 'C'= 'X'

    draw 'A' = 'X'
    draw 'B' = 'Y'
    draw 'C' = 'Z'

    lose 'A' = 'Z'
    lose 'B' = 'X'
    lose 'C' = 'Y'

p2part1 :: IO ()
p2part1 =
    sum
  . (score . pairs <$>)
  . lines <$> readFile "input2.txt" >>= print
  where
    pairs line = (head line, line !! 2)

p2part2 :: IO ()
p2part2 =
    sum
  . (score' . pairs <$>)
  . lines <$> readFile "input2.txt" >>= print
  where
    pairs line = (head line, line !! 2)

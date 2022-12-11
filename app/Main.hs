module Main (main) where

import Lib

main :: IO ()
main =
  sequence_ [
              p1part1, p1part2,
              p2part1, p2part2,
              p3part1, p3part2,
              p4part1, p4part2,
              p5part1, p5part2,
              p6part1, p6part2
            ]

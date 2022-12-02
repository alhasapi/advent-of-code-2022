module Lib where



main :: IO ()
main =
  fst
  . maxDeer
  . zip [1..]
  . (sum <$>)
  . mtakeW [] []
  . lines <$> readFile "input.txt" >>= print
  where
    mtakeW acc _    []      = acc
    mtakeW acc acc' ("":xs) = mtakeW (acc ++ [(read :: String -> Integer) <$> acc']) [] xs
    mtakeW acc acc' (x:xs)  = mtakeW acc (x:acc') xs

    maxDeer :: [(Integer, Integer)] -> (Integer, Integer)
    maxDeer = foldl1 predicate

    predicate :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
    predicate (idx, valx) (idy, valy) | valx > valy = (idx, valx)
                                      | otherwise   = (idy, valy)

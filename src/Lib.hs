module Lib where



main :: IO ()
main =
  zip [1..] . (sum <$>) . mtakeW [] [] . lines <$> readFile "input.txt" >>= mapM_ print
  where
    mtakeW acc _    [] = acc
    mtakeW acc acc' ("":xs) = mtakeW (acc ++ [(read :: String -> Integer) <$> acc']) [] xs
    mtakeW acc acc' (x:xs)  = mtakeW acc (x:acc') xs


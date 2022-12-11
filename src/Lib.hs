module Lib where

import qualified Data.List as L
import Data.Ord
import Data.Char
import qualified Data.Set as S
import qualified Data.Map as M


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
    sum3
  . L.sortOn Down
  . (sum <$>)
  . mtakeW [] []
  . lines <$> readFile "input.txt" >>= print
  where
    sum3 :: Num a
         => [a]
         -> a
    sum3 (x1:x2:x3:_) = sum [x1, x2, x3]
    sum3 _            = 0
    mtakeW :: [[Integer]]
           -> [String]
           -> [String]
           -> [[Integer]]
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
    _   -> error "Fatal error"

  where
    val 'X' = 1
    val 'Y' = 2
    val 'Z' = 3
    val  _  = error "Fatal error"

    win 'A'= 'Y'
    win 'B'= 'Z'
    win 'C'= 'X'
    win  _  = error "Fatal error"

    draw 'A' = 'X'
    draw 'B' = 'Y'
    draw 'C' = 'Z'
    draw  _  = error "Fatal error"

    lose 'A' = 'Z'
    lose 'B' = 'X'
    lose 'C' = 'Y'
    lose  _  = error "Fatal error"

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
    pairs :: [b] -> (b, b)
    pairs line = (head line, line !! 2)

p3part1 :: IO ()
p3part1 =
      sum
    . ( priority
      . head
      . uncurry L.intersect
      . split
        <$>
      )
    . lines <$> readFile "input3.txt" >>= print
  where
    split :: [a]
          -> ([a], [a])
    split line = splitAt (length line `div` 2) line

priority :: Char -> Int
priority letter =
  if ord letter >= ord 'a' && ord letter <= ord 'z' then
      ord letter - ord 'a' + 1
  else
      ord letter + 27 - ord 'A'

p3part2 :: IO ()
p3part2 =
      sum
    . (  priority
       . head
       . foldl1 L.intersect
        <$>
      )
    . by3 []
    . lines <$> readFile "input3.txt" >>= print
  where
    by3 :: [[a]]
        -> [a]
        -> [[a]]
    by3 acc  []           = acc
    by3 acc (x1:x2:x3:xs) = by3 ([x1,x2,x3]:acc) xs
    by3 acc ys = ys:acc

p4part1 :: IO ()
p4part1 =
        length
      . filter isFullyContained
      . (  map (read :: String -> Int)
         . lines
         . (fn <$>) <$>
        )
      . lines <$> readFile "input4.txt" >>= print
  where
    isFullyContained :: Ord q
                     => [q]
                     -> Bool
    isFullyContained [from, to, from', to'] =
        (from' >= from && to' <= to && to' >= from)
      || (from >= from' && to <= to' && to >= from')
    isFullyContained _ = undefined

    fn q =
      if q `elem` ['-', ','] then
        '\n'
      else
        q


p4part2 :: IO ()
p4part2 =
        length
      . filter overlap
      . (  map (read :: String -> Int)
         . lines
         . (fn <$>) <$>
        )
      . lines <$> readFile "input4.txt" >>= print
  where
    overlap :: Ord q
            => [q]
            -> Bool
    overlap [from, to, from', to'] =
        (from >= from' && from <= to') || (from'>= from  && from' <= to)
    overlap _ = undefined

    fn q =
      if q `elem` ['-', ','] then
        '\n'
      else
        q

p5part1 :: IO ()
p5part1 = do
  (objects, (_:operations)) <- cut . lines <$> readFile "input5.txt"
  let cstacks = clean $ init objects
  let stacks = (filter (/= "") $ filter (/= ' ') <$> L.transpose cstacks)
  let ops = parseOp operations
  print stacks
  print $ map head $ foldl move stacks ops
  where
     clean = (map (\q -> if  q `elem` "[]" then ' ' else  q) <$>)

cut :: [String]
      -> ([String], [String])
cut items =
  case L.elemIndex "" items of
    (Just idx) -> splitAt idx items
    Nothing    -> error "No occurrence of empty string found."

parseOp :: Functor f
        => f String
        -> f [Int]
parseOp ops =
    map (read :: String -> Int)
  . filter (`notElem` ["move", "to", "from"])
  . words
  <$> ops

slice :: Int
      -> Int
      -> [a]
      -> [a]
slice from to list = [
    list !! i | i <- [from..to]
  ]

change :: Int
       -> a
       -> [a]
       -> [a]
change index with list =
  slice 0 (index - 1) list
    ++ [with] ++
  slice (index + 1) (length list - 1) list
-- change index with list = [
--   list !! i | i <- [0..index-1]
--   ] ++ [with] ++ [
--     list !! i | i <- [index+1..length list - 1]]

move :: [String]
     -> [Int]
     -> [String]
move stacks [0, _, _]          = stacks
move stacks [number, from, to] =
  let istack = stacks !! (from - 1)
      dstack = stacks !! (to - 1)
      newIStack = tail istack
      target    = head istack
      newDStack = target:dstack
      updatedIStack = change (from - 1) newIStack stacks
      updatedDStack = change (to - 1)  newDStack updatedIStack
    in move updatedDStack [number - 1, from,  to]
move _ _ = error "shouldn't happend."

debugfoldl :: (a -> t -> a)
           -> a
           -> [t]
           -> [a]
           -> (a, [a])
debugfoldl _ init' [] acc = (init', acc)
debugfoldl fn init' (x:xs) acc
  = debugfoldl fn (fn init' x) xs (acc ++ [fn init' x])


p5part2 :: IO ()
p5part2 = do
  (objects, (_:operations)) <- cut . lines <$> readFile "input5.txt"
  let cstacks = clean $ init objects
  let stacks = (filter (/= "") $ filter (/= ' ') <$> L.transpose cstacks)
  let ops = parseOp operations
  print stacks
  --print $ map head $ debugfoldl move' stacks ops []
  let (res, _) = debugfoldl move' stacks ops []
  print $ map head res

  where
     clean = (map (\q -> if  q `elem` "[]" then ' ' else  q) <$>)

     move' :: [String]
          -> [Int]
          -> [String]
     move' stacks [0, _, _]             = stacks
     move' stacks instruction@[1, _, _] = move stacks instruction
     move' stacks [number, from, to]    =
       let istack = stacks !! (from - 1)
           dstack = stacks !! (to - 1)

           targets = slice 0 (number - 1) istack

           newIStack = slice number (length istack - 1) istack
           newDStack = targets ++ dstack

           updatedIStack = change (from - 1) newIStack stacks
           updatedDStack = change (to   - 1) newDStack updatedIStack
        in updatedDStack
     move' _ _  =
       error "The Unexpected Happened Anyway: the second argument must always be of length 3" -- should be handled while parsing.

-- look for a sequence of distinct letters
markerPosition :: Ord a
               => Int
               -> [a]
               -> Int
markerPosition idx stream =
  let target = slice idx (idx + 3) stream
  in if isDistinct target then
    idx + 4
   else
    markerPosition (idx + 1) stream

messagePosition :: Ord a
               => Int
               -> [a]
               -> Int
messagePosition idx stream =
  let target = slice idx (idx + 13) stream
  in if isDistinct target then
    idx + 14
   else
    messagePosition (idx + 1) stream

isDistinct :: Ord q
           => [q]
           -> Bool
isDistinct xs =
  length xs == length (S.toList . S.fromList $ xs)

p6part1 :: IO ()
p6part1 =
  markerPosition 0 <$> readFile "input6.txt" >>= print

p6part2 :: IO ()
p6part2 =
  messagePosition 0 <$> readFile "input6.txt" >>= print


p7part1 = do
   ast <- parse <$> readFile "input7.txt"
   let (_, dc) = fn ast
   print $ sum $ filter (<=100000) (snd <$> M.toList dc)
   where
    fn = exec [] emptyMap []
       . (++ [["cd", ".."]])
       . (++ [["cd", ".."]])
       . (++ [["cd", ".."]])

    parse =
         map words
       . lines
       . map (\z -> if z == '$' then ' ' else z)

toInt :: String -> Int
toInt = read

emptyMap :: M.Map String Int
emptyMap = M.fromList []

exec _     dc astack  [] = (astack, dc)
exec stack dc astack (["cd", ".."]:xs) = exec (init stack) newDc astack xs
  where
    newStack = init stack
    newDc    =
      let dr' = L.intercalate "/" stack
          dr = L.intercalate "/" newStack
      in case (M.lookup dr' dc, M.lookup dr dc) of
        (Just val, Just val') -> M.insert dr (val + val') dc
        (Just val, Nothing)   -> M.insert dr val dc
        _                     -> dc
exec stack dc astack (["cd",  dr]:xs)  =
  if currentStack `notElem` astack then
    exec currentStack newDc (currentStack:astack) xs
  else
    exec currentStack newDc astack xs
  where
    currentStack = stack ++ [dr]
    newDc =
      let dr' = L.intercalate "/" currentStack
      in M.insert dr' 0 dc

exec stack dc astack (["ls"]:xs)      = exec stack dc astack xs
exec stack dc astack (["dir",  _]:xs) = exec stack dc astack xs
exec stack dc astack ([size,  _]:xs)
  = let dr    = L.intercalate "/" stack
        size' = toInt size
    in case M.lookup dr dc of
        (Just val) -> exec stack (M.insert dr (size'+val) dc) astack xs
        Nothing    -> exec stack (M.insert dr size' dc) astack xs

p7part2  :: IO ()
p7part2  = do
   ast <- parse <$> readFile "input7.txt"
   let (astack, dc) = fn ast
   let (Just maxSize) = M.lookup "/" dc
   let freeSpace = 70000000 - maxSize
   print freeSpace
   print maxSize
   print $ minimum $ filter (\q ->  freeSpace + q >= 30000000) (snd <$> M.toList dc)

   where
    fn = exec [] emptyMap []
       . (++ [["cd", ".."]])
       . (++ [["cd", ".."]])
       . (++ [["cd", ".."]])

    parse =
         map words
       . lines
       . map (\z -> if z == '$' then ' ' else z)

import Data.List (nub)
import Data.Map qualified as M
import Math.Combinat.Sets
import System.Environment

filename :: [String] -> String
filename (a : _) = a ++ ".txt"
filename _ = "in.txt"

main :: IO ()
main = do
  args <- getArgs
  let name = filename args
  contents <- readFile name
  let grid = lines contents
  let p1 = part1 grid
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 grid
  putStrLn ("Part 2: " ++ show p2)

enumerate = zip [0 ..]

antinodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes (x, y) (u, v) = let (dx, dy) = (u - x, v - y) in [(x - dx, y - dy), (u + dx, v + dy)]

allAntinodes :: (t -> t -> [b]) -> [t] -> [b]
allAntinodes f = concatMap (\[x, y] -> f x y) . choose 2

solve :: [[Char]] -> ((Int, Int) -> (Int, Int) -> [(Int, Int)]) -> Int
solve grid f =
  length $
    filter (\(x, y) -> x >= 0 && x < w && y >= 0 && y < h) $
      nub $
        concatMap (allAntinodes f) grouped
  where
    (w, h) = (length $ head grid, length grid)
    antennas =
      concat [[(c, (x, y)) | (x, c) <- enumerate row, c /= '.'] | (y, row) <- enumerate grid]
    grouped = M.elems $ foldr (\(k, v) acc -> M.insertWith (++) k [v] acc) M.empty antennas

part1 :: [String] -> Int
part1 grid = solve grid antinodes

antinodes2 :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
antinodes2 (x, y) (u, v) =
  let (dx, dy) = (u - x, v - y)
   in [(x - i * dx, y - i * dy) | i <- [0 .. 100]]
        ++ [(u + i * dx, v + i * dy) | i <- [0 .. 100]]

part2 :: [String] -> Int
part2 grid = solve grid antinodes2
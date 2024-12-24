import Data.List (group, groupBy, maximumBy, sort, sortBy)
import System.Environment

filename :: [String] -> String
filename (a : _) = a ++ ".txt"
filename _ = "in.txt"

main :: IO ()
main = do
  args <- getArgs
  let name = filename args
  contents <- readFile name
  let ls = lines contents
  let p1 = part1 ls
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 ls
  putStrLn ("Part 2: " ++ show p2)

data Type = Incr | Decr | Eq

monotonic :: Type -> Bool -> Int -> [Int] -> Bool
monotonic _ _ _ [] = True
monotonic _ True prev (x : _) | abs (prev - x) > 3 || abs (prev - x) < 1 = False
monotonic ty False prev (x : xs) | abs (prev - x) > 3 || abs (prev - x) < 1 = monotonic ty True prev xs
monotonic Eq True prev (x : xs) | prev < x = monotonic Incr True x xs
monotonic Eq False prev (x : xs) | prev < x = monotonic Incr False x xs || monotonic Eq True prev xs
monotonic Eq True prev (x : xs) | prev > x = monotonic Decr True x xs
monotonic Eq False prev (x : xs) | prev > x = monotonic Decr False x xs || monotonic Eq True prev xs
monotonic Eq skip prev (x : xs) | prev == x = monotonic Eq skip x xs
monotonic Incr skip prev (x : xs) | prev <= x = monotonic Incr skip x xs
monotonic Decr skip prev (x : xs) | prev >= x = monotonic Decr skip x xs
monotonic _ True _ _ = False
monotonic ty False prev (x : xs) = monotonic ty True prev xs

part1 :: [String] -> Int
part1 ls = length $ filter (f . map read . words) ls
  where
    f (x : xs) = monotonic Eq True x xs

part2 :: [String] -> Int
part2 ls = length $ filter (f . map read . words) ls
  where
    f (x1 : x2 : xs) = monotonic Eq False x1 (x2 : xs) || monotonic Eq True x2 xs
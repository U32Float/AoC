import Data.List (sort)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

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

tuple :: [a] -> (a, a)
tuple [x, y] = (x, y)

absdiff :: Int -> Int -> Int
absdiff x y = abs (x - y)

part1 :: [String] -> Int
part1 ls = sum $ zipWith absdiff (sort left) (sort right)
  where
    (left, right) = unzip $ map (tuple . map read . words) ls

count :: (Ord a) => [a] -> M.Map a Int
count xs = M.fromListWith (+) [(x, 1) | x <- xs]

part2 :: [String] -> Int
part2 ls = sum $ map (\x -> x * fromMaybe 0 (M.lookup x cs)) left
  where
    (left, right) = unzip $ map (tuple . map read . words) ls
    cs = count right
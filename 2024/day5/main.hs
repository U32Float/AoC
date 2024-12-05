import Data.List (groupBy, intersect, sortBy)
import Data.List.Split (split, splitOn)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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

type Rules = Map Int [Int]

parseRules :: [String] -> [(Int, Int)]
parseRules [] = []
parseRules (l : ls) = let [lhs, rhs] = splitOn "|" l in (read lhs, read rhs) : parseRules ls

parse :: [String] -> (Rules, [[Int]])
parse ls = (getRules (parseRules rls) M.empty, map (map read . splitOn ",") uls)
  where
    [rls, _, uls] = groupBy (\a b -> a /= "" && b /= "") ls

getRules :: [(Int, Int)] -> Rules -> Rules
getRules [] m = m
getRules ((lhs, rhs) : rs) m = getRules rs (M.insert rhs (lhs : fromMaybe [] (M.lookup rhs m)) m)

isValid :: Rules -> [Int] -> Bool
isValid _ [] = True
isValid rules (x : xs) = case M.lookup x rules of
  Just rs -> null (xs `intersect` rs) && isValid rules xs
  Nothing -> isValid rules xs

part1 :: [String] -> Int
part1 ls = sum . map (\u -> u !! (length u `div` 2)) $ filter (isValid rules) updates
  where
    (rules, updates) = parse ls

sort :: Rules -> Int -> Int -> Ordering
sort rules a b
  | b `elem` fromMaybe [] (M.lookup a rules) = GT
  | a `elem` fromMaybe [] (M.lookup b rules) = LT
  | otherwise = EQ

part2 :: [String] -> Int
part2 ls =
  sum
    ( map
        ((\u -> u !! (length u `div` 2)) . sortBy (sort rules))
        (filter (not . isValid rules) updates)
    )
  where
    (rules, updates) = parse ls
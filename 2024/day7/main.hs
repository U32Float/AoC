import Data.Either (rights)
import Data.List.Split (splitOn)
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)

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

nums :: Parser (Int, [Int])
nums = do
  target <- read <$> many1 digit
  _ <- string ": "
  xs <- sepBy1 (read <$> many1 digit) (char ' ')
  return (target, xs)

type Op = Int -> Int -> Int

(|||) :: Op
(|||) lhs rhs = read (show lhs ++ show rhs)

check :: [Op] -> Int -> Int -> [Int] -> Bool
check _ target t [] = target == t
check ops target t (x : xs) = any (\op -> check ops target (t `op` x) xs) ops

part1 :: [String] -> Int
part1 ls = sum $ map fst $ filter (\(t, x : xs) -> check [(+), (*)] t x xs) $ rights $ map (parse nums "") ls

part2 :: [String] -> Int
part2 ls = sum $ map fst $ filter (\(t, x : xs) -> check [(+), (*), (|||)] t x xs) $ rights $ map (parse nums "") ls
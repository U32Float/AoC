import Control.Monad (void)
import Data.Char
import Data.Either
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String

filename :: [String] -> String
filename (a : _) = a ++ ".txt"
filename _ = "in.txt"

main :: IO ()
main = do
  args <- getArgs
  let name = filename args
  contents <- readFile name
  let ls = lines contents
  let p1 = part1 $ concat ls
  putStrLn ("Part 1: " ++ show p1)
  let p2 = part2 $ concat ls
  putStrLn ("Part 2: " ++ show p2)

type Mul = (Int, Int)

mul :: Parser Mul
mul = do
  _ <- string "mul("
  lhs <- many1 (satisfy isDigit)
  _ <- string ","
  rhs <- many1 (satisfy isDigit)
  _ <- string ")"
  return (read lhs, read rhs)

findall :: Parser [Maybe Mul]
findall = many $ (Just <$> try mul) <|> (Nothing <$ anyChar)

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . catMaybes . fromRight [] . parse findall ""

enable = "do()"

disable = "don't()"

data Cond = Enabled String | Disabled String deriving (Show)

inner :: Parser String
inner = manyTill anyChar (lookAhead (void (try (string disable) <|> string enable) <|> eof))

enabled :: Parser Cond
enabled = do
  _ <- string enable
  Enabled <$> inner

disabled :: Parser Cond
disabled = do
  _ <- string disable
  Disabled <$> inner

conds :: Parser [Cond]
conds = do
  start <- inner
  cs <- many (try disabled <|> enabled)
  return (Enabled start : cs)

calc :: Cond -> Int
calc (Enabled s) = part1 s
calc (Disabled _) = 0

part2 :: String -> Int
part2 = sum . map calc . fromRight [] . parse conds ""

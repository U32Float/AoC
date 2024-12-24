import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace (trace)
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
  let path = part1 grid
  putStrLn ("Part 1: " ++ show (S.size path))
  let p2 = part2 grid path
  putStrLn ("Part 2: " ++ show p2)

turnRight :: (Int, Int) -> (Int, Int)
turnRight (0, -1) = (1, 0)
turnRight (1, 0) = (0, 1)
turnRight (0, 1) = (-1, 0)
turnRight (-1, 0) = (0, -1)

walk :: [String] -> (Int, Int) -> (Int, Int) -> Set (Int, Int)
walk grid (x, y) (dx, dy)
  | x + dx < 0
      || x + dx >= length (head grid)
      || y + dy < 0
      || y + dy >= length grid =
      S.singleton (x, y)
walk grid (x, y) (dx, dy)
  | grid !! (y + dy) !! (x + dx) == '#' =
      walk grid (x, y) (turnRight (dx, dy))
walk grid (x, y) (dx, dy) =
  S.insert (x, y) (walk grid (x + dx, y + dy) (dx, dy))

enumerate = zip [0 ..]

part1 :: [String] -> Set (Int, Int)
part1 grid = walk grid start (0, -1)
  where
    start =
      head $
        concat $
          [ [(x, y) | (x, c) <- enumerate line, c == '^']
            | (y, line) <- enumerate grid
          ]

isLoop ::
  [String] ->
  Set ((Int, Int), (Int, Int)) ->
  (Int, Int) ->
  (Int, Int) ->
  (Int, Int) ->
  Bool
isLoop grid vs (x, y) (dx, dy) (ox, oy)
  | x + dx < 0
      || x + dx >= length (head grid)
      || y + dy < 0
      || y + dy >= length grid =
      False
isLoop grid vs (x, y) (dx, dy) _
  | S.member ((x, y), (dx, dy)) vs = True
isLoop grid vs (x, y) (dx, dy) (ox, oy)
  | (grid !! (y + dy) !! (x + dx) == '#') || (x + dx == ox && y + dy == oy) =
      isLoop grid (S.insert ((x, y), (dx, dy)) vs) (x, y) (turnRight (dx, dy)) (ox, oy)
isLoop grid vs (x, y) (dx, dy) (ox, oy) =
  isLoop grid (S.insert ((x, y), (dx, dy)) vs) (x + dx, y + dy) (dx, dy) (ox, oy)

part2 :: [String] -> Set (Int, Int) -> Int
part2 grid path = S.size $ S.filter (isLoop grid S.empty start (0, -1)) path
  where
    start =
      head $
        concat $
          [ [(x, y) | (x, c) <- enumerate line, c == '^']
            | (y, line) <- enumerate grid
          ]
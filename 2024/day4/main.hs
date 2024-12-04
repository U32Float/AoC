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

contains :: [String] -> (Int, Int) -> Bool
contains grid (x, y) = x >= 0 && x < w && y >= 0 && y < h
  where
    h = length grid
    w = length (head grid)

find :: [String] -> Int -> (Int, Int) -> (Int, Int) -> Bool
find grid 3 (x, y) _ | grid !! y !! x == 'S' = True
find grid i (x, y) (dx, dy)
  | contains grid (x + dx, y + dy) && grid !! y !! x == "XMAS" !! i =
      find grid (i + 1) (x + dx, y + dy) (dx, dy)
find _ _ _ _ = False

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, 1), (1, -1), (-1, -1)]

enumerate = zip [0 ..]

part1 :: [String] -> Int
part1 grid = sum $ map fromEnum $ concat $ concat mask
  where
    mask =
      [ [ [find grid 0 (x, y) dir | dir <- dirs] | (x, c) <- enumerate line, c == 'X'
        ]
        | (y, line) <- enumerate grid
      ]

findXMAS :: [String] -> (Int, Int) -> Bool
findXMAS grid (x, y) | x - 1 < 0 || x + 1 >= w || y - 1 < 0 || y + 1 >= h = False
  where
    h = length grid
    w = length (head grid)
findXMAS grid (x, y) =
  (grid !! (y - 1) !! (x - 1) == 'S' || grid !! (y - 1) !! (x - 1) == 'M')
    && (grid !! (y + 1) !! (x + 1) == 'S' || grid !! (y + 1) !! (x + 1) == 'M')
    && (grid !! (y - 1) !! (x - 1) /= grid !! (y + 1) !! (x + 1))
    && (grid !! (y - 1) !! (x + 1) == 'S' || grid !! (y - 1) !! (x + 1) == 'M')
    && (grid !! (y + 1) !! (x - 1) == 'S' || grid !! (y + 1) !! (x - 1) == 'M')
    && (grid !! (y - 1) !! (x + 1) /= grid !! (y + 1) !! (x - 1))

part2 :: [String] -> Int
part2 grid = sum $ map fromEnum $ concat mask
  where
    mask =
      [ [findXMAS grid (x, y) | (x, c) <- enumerate line, c == 'A']
        | (y, line) <- enumerate grid
      ]
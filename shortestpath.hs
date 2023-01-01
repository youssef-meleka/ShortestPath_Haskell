{-
Example input:
50 10 30 5 90 20 40 2 25 10 8 0 runhaskell shortestpath.hs
Example output:
Optimal path: B10 C30 A5 C20 B2 B8 C0
Optimal path length: 75
-}
import Data.Function (on)
import Data.List     (minimumBy)
--
-- Road system can be represented as a list of road sections
type RoadSystem = [Section]
--
-- Triple of paths (A, B, C) and their lengths describe each road section
data Section = Section Int Int Int deriving Show
--
-- represents type of path segment
-- A: move forward by path A
-- B: move forward by path B
-- C: turn on crossroad
data Label = A | B | C deriving (Show)
--
-- Path can be represented by list of path segments
type Path = [PathSegment]
--
-- Path segment represents an individual move,
-- including label of path segment and it's length
type PathSegment = (Label, Int)
--
-- entry point
main :: IO ()
main = do
  input <- readInput
  let road = parseRoadSystem input
  let path = optimalPath road
  printOptimalPath path
--
-- given RoadSystem, calculate optimal Path
optimalPath :: RoadSystem -> Path
optimalPath road =
  let (pathA, pathB) = foldl walkRoadSection ([], []) road
  in reverse $ minimumBy (compare `on` pathLength) [pathA, pathB]
--
-- given previous best A and B paths
-- and next road section a
-- finds next best A and B path
walkRoadSection :: (Path, Path) -> Section -> (Path, Path)
walkRoadSection (pathA, pathB) (Section a b c) =
  let pathALength = pathLength pathA
      pathBLength = pathLength pathB
      priceAfromA = pathALength + a
      priceAfromB = pathBLength + b + c
      priceBfromA = pathALength + a + c
      priceBfromB = pathBLength + b
      nextPathA =
        if priceAfromA < priceAfromB then (A,a):pathA
        else (C,c):(B,b):pathB
      nextPathB =
        if priceBfromB < priceBfromA then (B,b):pathB
        else (C,c):(A,a):pathA
  in (nextPathA, nextPathB)
--
-- get total path length
pathLength :: Path -> Int
pathLength = sum . map snd
--
-- convert path segment to user-friendly string
showPathSegment :: PathSegment -> String
showPathSegment (label, len) = show label ++ show len
-- parse user input into RoadSystem
parseRoadSystem :: [Int] -> RoadSystem
parseRoadSystem = map (\[a, b, c] -> (Section a b c)) . groupsOf 3
---------------------------
readInput :: IO [Int]
readInput = do
  x <- getLine
  if null x then return []
  else do
    y <- readInput
    let number = read x :: Int
    return (number:y)
-- print info about given optimal path
printOptimalPath :: Path -> IO ()
printOptimalPath path = do
  putStrLn $ "Optimal path: " ++ unwords (map showPathSegment path)
  putStrLn $ "Optimal path length: " ++ show (pathLength path)
--
-- splits list of things into a list of groups of a given length
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = error "Cannot make groups of length 0"
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
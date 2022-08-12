{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Functor.Compose
import Data.Grid
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Lens.Micro
import Lens.Micro.Extras (view)
import System.Random (randomRIO)

data TileType = Mine | Num Int | Empty
data TileStatus = Hidden | Free
data Tile = Tile TileStatus TileType

type GridSize = '[10,10]
type Field = Grid GridSize
type MineField = Field Tile

data InitTile = InitMine | InitEmpty
  deriving Eq
type InitMineField = Field InitTile

-- from https://www.programming-idioms.org/idiom/158/random-sublist/2123/haskell
randomSample ::Int -> [a] -> IO [a]
randomSample 0 x = pure []
randomSample k x = do
   i <- randomRIO (0, length x - 1)
   let (a, e:b) = splitAt i x
   l <- randomSample (k-1) (a ++ b)
   pure (e : l)


--------------------
-- Initialization --
--------------------

emptyGrid :: InitMineField
emptyGrid = generate (const InitEmpty)


linspace :: [Int] -> [[Int]]
linspace [] = error "Need at least one dimension for the linspace"
linspace (x:[]) = [ [a] | a <- [0..x-1] ]
linspace (x:xs) = map (flip (:)) (linspace xs) <*> [0..x-1]


fillWithMines :: Int -> InitMineField -> IO InitMineField
fillWithMines numMines grid = do
  mineCoords <- randomSample numMines allCoords
  pure (grid // zip mineCoords (repeat InitMine))
  where
    -- Get the coordinate space dimentions from the GridSize
    dims :: [Int]
    dims = unCoord (maxBound :: Coord GridSize)

    allCoords :: [Coord GridSize]
    allCoords = fromJust . traverse coord . linspace $ dims


calcNeighbors :: InitMineField -> MineField
calcNeighbors = autoConvolute omitBounds (Tile Hidden . go)
  where
    center :: Grid '[3,3] (Maybe a) -> a
    center = fromJust . view (cell (fromJust $ coord [1,1]))

    countMines :: Grid '[3,3] (Maybe InitTile) -> Int
    countMines = length . filter (== Just InitMine) . mconcat . toNestedLists

    go :: Compose (Grid '[3,3]) Maybe InitTile -> TileType
    go (getCompose -> neigh) =
      case center neigh of
        InitMine -> Mine
        InitEmpty ->
          let n = countMines neigh
          in if n == 0 then Empty else Num n

initGrid :: Int -> IO MineField
initGrid numMines = fmap calcNeighbors $ fillWithMines numMines emptyGrid

showGrid :: MineField -> String
showGrid = intercalate "\n" . map showRow . toNestedLists
  where
    showRow :: [Tile] -> String
    showRow = mconcat . map showTile

    showTile :: Tile -> String
    showTile (Tile Hidden _) = "#"
    showTile (Tile Free t) = showTileType t

    showTileType :: TileType -> String
    showTileType Mine = "*"
    showTileType (Num i) = show i
    showTileType Empty = "-"

freeAllTiles :: MineField -> MineField
freeAllTiles = fmap (\(Tile _ x) -> Tile Free x)

main :: IO ()
main = do
  grid <- initGrid 10
  putStrLn (showGrid grid)
  putStrLn ""
  putStrLn (showGrid (freeAllTiles grid))

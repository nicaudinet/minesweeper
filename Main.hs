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

-- Assumed to be 2D
type GridSize = '[10,10]
type Field = Grid GridSize
type MineField = Field Tile

data InitTile = InitMine | InitEmpty
  deriving Eq
type InitMineField = Field InitTile

emptyGrid :: InitMineField
emptyGrid = generate (const InitEmpty)

-- from https://www.programming-idioms.org/idiom/158/random-sublist/2123/haskell
randomSample ::Int -> [a] -> IO [a]
randomSample 0 x = pure []
randomSample k x = do
   i <- randomRIO (0, length x - 1)
   let (a, e:b) = splitAt i x
   l <- randomSample (k-1) (a ++ b)
   pure (e : l)

-- Assuming a 2D minefield
fillWithMines :: Int -> InitMineField -> IO InitMineField
fillWithMines numMines grid = do
  mineCoords <- randomSample numMines allCoords
  pure (grid // zip mineCoords (repeat InitMine))
  where
    max1, max2 :: Int
    max1 = unCoord (maxBound :: Coord GridSize) !! 0
    max2 = unCoord (maxBound :: Coord GridSize) !! 1

    allCoords :: [Coord GridSize]
    allCoords = fromJust $ sequence [ (coord [x,y]) | x <- [0..max1], y <- [0..max2] ]

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

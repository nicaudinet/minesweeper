{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Functor.Compose
import Data.Grid
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.Set as S
import Lens.Micro
import Lens.Micro.Extras (view)
import System.Random (randomRIO)

data TileType = Mine | Num Int | Empty
data TileStatus = Hidden | Free
data Tile = Tile TileStatus TileType

type GridSize = '[50,50]
type Field = Grid GridSize
type MineField = Field Tile

data InitTile = InitMine | InitEmpty
  deriving Eq
type InitMineField = Field InitTile

-- Needed in order to keep the coordinates in a set
instance Ord (Coord dims) where
  compare (Coord a) (Coord b) = compare a b

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
linspace [] = []
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
    countMines :: Grid '[3,3] (Maybe InitTile) -> Int
    countMines = length . filter (== Just InitMine) . mconcat . toNestedLists

    go :: Compose (Grid '[3,3]) Maybe InitTile -> TileType
    go (getCompose -> neigh) =
      case fromJust (view (cell centerCoord) neigh) of
        InitMine -> Mine
        InitEmpty ->
          let n = countMines neigh
          in if n == 0 then Empty else Num n


initGrid :: Int -> IO MineField
initGrid numMines = fmap calcNeighbors $ fillWithMines numMines emptyGrid


------------
-- Update --
------------

type CoordSet = S.Set (Coord GridSize)

free :: Coord GridSize -> MineField -> MineField
free coord grid =
  let Tile _ value = grid ^?! cell coord
  in grid // [(coord, Tile Free value)]

neighbors :: Coord GridSize -> [Coord GridSize]
neighbors = catMaybes . map coord . neighborCoords . unCoord
  where
    neighborCoords :: [Int] -> [[Int]]
    neighborCoords [] = []
    neighborCoords (x:[]) = [ [a] | a <- [x-1, x, x+1] ]
    neighborCoords (x:xs) = map (flip (:)) (neighborCoords xs) <*> [x-1, x, x+1]


click :: Coord GridSize -> MineField -> MineField
click coord grid =
  case grid ^?! cell coord of
    Tile _ Mine -> free coord grid
    otherwise -> foldr free grid coords
  where
    f :: CoordSet -> CoordSet -> CoordSet
    f look seen =
      if S.null look
      then seen
      else uncurry f (foldr g (S.empty, seen) look)

    g :: Coord GridSize -> (CoordSet, CoordSet) -> (CoordSet, CoordSet)
    g c (look, seen) =
      if S.member c seen
      then (look, seen)
      else
        case grid ^?! cell c of
          Tile _ Empty ->
            let ns = S.delete c . S.fromList $ neighbors c
            in (S.union look ns, S.insert c seen)
          otherwise -> (look, S.insert c seen)

    coords :: CoordSet
    coords = f (S.singleton coord) S.empty


----------
-- Main --
----------

showGrid :: MineField -> String
showGrid = intercalate "\n" . map showRow . toNestedLists
  where
    showRow :: [Tile] -> String
    showRow = intercalate " " . map showTile

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
  grid <- initGrid 100
  -- putStrLn (showGrid (freeAllTiles grid))
  let grid' = click (fromJust $ coord [1,1]) grid
  putStrLn (showGrid grid')

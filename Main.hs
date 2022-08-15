{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Loops (untilM_)
import Control.Monad.State
import Data.Functor.Compose
import Data.Generics.Product.Fields (field)
import Data.Grid
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.Set as S
import GHC.Generics
import Lens.Micro
import Lens.Micro.Extras (view)
import Miso
import qualified Miso.String as M
import System.Random (randomRIO)
import Text.Read (readMaybe)

-----------
-- Types --
-----------

data TileType = Mine | Num Int | Empty
  deriving Eq

data TileStatus = Hidden | Free
  deriving Eq

data Tile = Tile
  { tileStatus :: TileStatus
  , tileType :: TileType
  }
  deriving (Generic, Eq)

-- Assumed to be 2D
type GridSize = '[5,5]
type Field = Grid GridSize
type MineField = Field Tile

data InitTile = InitMine | InitEmpty
  deriving Eq
type InitMineField = Field InitTile

data Outcome = Undefined | Win | Lose
data GameState = GameState
  { minefield :: MineField
  , outcome :: Outcome
  }
  deriving Generic
type Game a = StateT GameState IO a

-- Needed in order to keep the coordinates in a set
instance Ord (Coord dims) where
  compare (Coord a) (Coord b) = compare a b


---------------
-- Utilities --
---------------

-- Assuming a 2D gridsize!
filterGrid :: (a -> Bool) -> Grid GridSize a -> [a]
filterGrid f = filter f . mconcat . toNestedLists


--------------------
-- Initialization --
--------------------

emptyGrid :: InitMineField
emptyGrid = generate (const InitEmpty)

linspace :: [Int] -> [[Int]]
linspace [] = []
linspace (x:[]) = [ [a] | a <- [0..x-1] ]
linspace (x:xs) = map (flip (:)) (linspace xs) <*> [0..x-1]

-- from https://www.programming-idioms.org/idiom/158/random-sublist/2123/haskell
randomSample ::Int -> [a] -> IO [a]
randomSample 0 x = pure []
randomSample k x = do
   i <- randomRIO (0, length x - 1)
   let (a, e:b) = splitAt i x
   l <- randomSample (k-1) (a ++ b)
   pure (e : l)

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

free :: Coord GridSize -> Game ()
free coord =
  let lens = field @"minefield" . cell coord . field @"tileStatus"
  in modify (over lens (const Free))

neighbors :: Coord GridSize -> [Coord GridSize]
neighbors = catMaybes . map coord . neighborCoords . unCoord
  where
    neighborCoords :: [Int] -> [[Int]]
    neighborCoords [] = []
    neighborCoords (x:[]) = [ [a] | a <- [x-1, x, x+1] ]
    neighborCoords (x:xs) = map (flip (:)) (neighborCoords xs) <*> [x-1, x, x+1]

inspect :: Coord GridSize -> Game Tile
inspect c = gets (^?! field @"minefield" . cell c)

click :: Coord GridSize -> Game ()
click coord = do
  tile <- inspect coord
  grid <- gets minefield
  case tile of
    Tile _ Mine -> free coord
    otherwise -> mapM_ free (coords grid)
  where
    f :: MineField -> CoordSet -> CoordSet -> CoordSet
    f grid look seen =
      if S.null look
      then seen
      else uncurry (f grid) (foldr (g grid) (S.empty, seen) look)

    g :: MineField -> Coord GridSize -> (CoordSet, CoordSet) -> (CoordSet, CoordSet)
    g grid c (look, seen) =
      if S.member c seen
      then (look, seen)
      else
        case grid ^?! cell c of
          Tile _ Empty ->
            let ns = S.delete c . S.fromList $ neighbors c
            in (S.union look ns, S.insert c seen)
          otherwise -> (look, S.insert c seen)

    coords :: MineField -> CoordSet
    coords grid = f grid (S.singleton coord) S.empty


---------------
-- Game Loop --
---------------

-- Assuming 2D GridSize
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

printMinefield :: Game ()
printMinefield = do
  grid <- gets minefield
  liftIO $ putStrLn (showGrid grid)

askUserInput :: Game (Coord GridSize)
askUserInput = do
  line <- liftIO getLine
  case readMaybe line of
    Nothing -> do
      liftIO $ putStrLn "Input could not be read, please try again"
      askUserInput
    Just list ->
      case coord list of
        Nothing -> do
          liftIO $ putStrLn "Input is out of bounds, please try again"
          askUserInput
        Just c -> pure c

mineIsUncovered :: MineField -> Bool
mineIsUncovered grid = length (filterGrid (== Tile Free Mine) grid) > 0

isMine :: Tile -> Bool
isMine (Tile _ Mine) = True
isMine _ = False

isCovered :: Tile -> Bool
isCovered (Tile Hidden _) = True
isCovered _ = False

fieldIsClear :: MineField -> Bool
fieldIsClear grid =
  let numMines = length (filterGrid isMine grid)
      numCovered = length (filterGrid isCovered grid)
  in numMines == numCovered

-- Game ends either when you click on a mine or everything but mines is free
updateGameState :: Game ()
updateGameState = do
  grid <- gets minefield
  if mineIsUncovered grid
  then do
    printMinefield
    modify (over (field @"outcome") (const Lose))
  else pure ()
  if fieldIsClear grid
  then do
    printMinefield
    modify (over (field @"outcome") (const Win))
  else pure ()

turn :: Game ()
turn = do
  printMinefield
  coord <- askUserInput
  click coord
  updateGameState

isGameOver :: Game Bool
isGameOver = gets outcome >>= \case
  Undefined -> pure False
  otherwise -> pure True

play :: Game ()
play = untilM_ turn isGameOver


----------
-- Main --
----------

main :: IO ()
main = do
  grid <- initGrid 2
  res <- execStateT play (GameState grid Undefined)
  case outcome res of
    Win -> putStrLn "Well done, you won!"
    Lose -> putStrLn "Oh no, you lost :("
    Undefined -> error "something bad happened"

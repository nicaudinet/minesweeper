{-# LANGUAGE DataKinds #-}

module Main where

import Data.Grid
import Data.List (intercalate)
import Data.Maybe (fromJust)

data TileType = Mine | Num Int | Empty
data TileStatus = Hidden | Free
data Tile = Tile TileStatus TileType

type GridSize = '[3,3]
type MineField = Grid GridSize Tile

initGrid :: IO MineField
initGrid = pure . fromJust . fromNestedLists $
  [[th1, th1, tf1]
  ,[th1, tm, tf1]
  ,[tf1, tf1, tf1]
  ]
  where
    th1 = Tile Hidden (Num 1)
    tf1 = Tile Free (Num 1)
    tm = Tile Hidden Mine

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
    showTileType Empty = "*"

printGrid :: MineField -> IO ()
printGrid = putStrLn . showGrid

main :: IO ()
main = initGrid >>= printGrid

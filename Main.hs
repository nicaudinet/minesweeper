module Main where

import Data.List (intercalate)

data TileType = Mine | Num Int | Empty
data TileStatus = Hidden | Free
data Tile = Tile TileStatus TileType
newtype Grid = Grid { unGrid :: [[Tile]] }

initGrid :: IO Grid
initGrid = pure $ Grid
  [[th1, th1, tf1]
  ,[th1, tm, tf1]
  ,[tf1, tf1, tf1]
  ]

  where
    th1 = Tile Hidden (Num 1)
    tf1 = Tile Free (Num 1)
    tm = Tile Hidden Mine

showGrid :: Grid -> String
showGrid = intercalate "\n" . map showRow . unGrid
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

printGrid :: Grid -> IO ()
printGrid = putStrLn . showGrid

main :: IO ()
main = initGrid >>= printGrid

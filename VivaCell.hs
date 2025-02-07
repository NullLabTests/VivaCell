{-# LANGUAGE RecordWildCards #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map.Strict as M
import qualified Data.Set as S

------------------------------------------------------------
-- Types and State
------------------------------------------------------------

type Cell = (Int, Int)
-- Each live cell is stored with its Color.
type World = M.Map Cell Color

data GameState = GameState
  { world      :: World   -- The grid of live cells with colors.
  , running    :: Bool    -- True if simulation is running.
  , generation :: Int     -- Generation counter.
  } deriving (Show)

------------------------------------------------------------
-- Constants and Window Setup
------------------------------------------------------------

cellSize :: Float
cellSize = 10

gridWidth, gridHeight :: Int
gridWidth  = 80
gridHeight = 60

windowWidth, windowHeight :: Int
windowWidth  = gridWidth  * (round cellSize)
windowHeight = gridHeight * (round cellSize)

fps :: Int
fps = 10

------------------------------------------------------------
-- Initial World (Randomly Populated)
------------------------------------------------------------

-- For each cell in the grid, with probability p, assign a random vibrant color.
initialWorldIO :: IO World
initialWorldIO = do
  let coords = [ (x, y) | x <- [0 .. gridWidth - 1], y <- [0 .. gridHeight - 1] ]
  let cellList = map (\cell -> if even (fst cell + snd cell) then Just (cell, vibrantColor) else Nothing) coords
  return $ M.fromList [ pair | Just pair <- cellList ]

vibrantColor :: Color
vibrantColor = makeColor 0.5 0.7 1.0 1  -- Some fixed vibrant color, can be changed.

------------------------------------------------------------
-- Game of Life with Colors
------------------------------------------------------------

-- Standard eight neighbors; we restrict to cells inside the grid.
neighbors :: Cell -> [Cell]
neighbors (x,y) = filter inBounds 
  [ (x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], not (dx == 0 && dy == 0) ]
  where
    inBounds (a, b) = a >= 0 && a < gridWidth && b >= 0 && b < gridHeight

liveNeighbors :: World -> Cell -> [(Cell, Color)]
liveNeighbors world cell =
  [ (c, col) | c <- neighbors cell, Just col <- [M.lookup c world] ]

-- Average a list of Colors (by averaging their RGBA components)
averageColor :: [Color] -> Color
averageColor colors =
  let toList c = let (r,g,b,a) = rgbaOfColor c in [r, g, b, a]
      sums = foldr1 (zipWith (+)) (map toList colors)
      n = fromIntegral (length colors)
      avgList = map (/ n) sums
  in case avgList of
       [r, g, b, a] -> makeColor r g b a
       _            -> white

-- Compute the next generation.
stepWorld :: World -> World
stepWorld world = M.fromList $ do
  -- Consider all cells that are alive or neighbor to an alive cell.
  let candidateCells = S.toList $ S.fromList $ concatMap (\(c, _) -> c : neighbors c) (M.toList world)
  cell <- candidateCells
  let nbs = liveNeighbors world cell
      count = length nbs
  if M.member cell world
     then if count == 2 || count == 3 then [(cell, world M.! cell)] else []
     else if count == 3
             then let cols = map snd nbs
                      newColor = averageColor cols
                  in [(cell, newColor)]
             else []

------------------------------------------------------------
-- Event Handling (Start/Stop Button)
------------------------------------------------------------

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) gs =
  if inButton mousePos then gs { running = not (running gs) } else gs
handleEvent _ gs = gs

-- The button is drawn in the upper-left corner.
inButton :: (Float, Float) -> Bool
inButton (mx, my) =
  let bx = -fromIntegral windowWidth/2 + 10
      by = fromIntegral windowHeight/2 - 60
      bw = 100
      bh = 50
  in mx >= bx && mx <= bx + bw && my >= by && my <= by + bh

------------------------------------------------------------
-- Simulation Update
------------------------------------------------------------

updateState :: Float -> GameState -> GameState
updateState _ gs
  | running gs = gs { world = stepWorld (world gs), generation = generation gs + 1 }
  | otherwise  = gs

------------------------------------------------------------
-- Rendering
------------------------------------------------------------

renderState :: GameState -> Picture
renderState gs = pictures [ drawWorld (world gs)
                            , drawButton (running gs)
                            , drawGeneration (generation gs)
                            ]

drawWorld :: World -> Picture
drawWorld world = pictures [ drawCell cell col | (cell, col) <- M.toList world ]

drawCell :: Cell -> Color -> Picture
drawCell (x, y) col =
  let xPos = fromIntegral x * cellSize - fromIntegral windowWidth/2 + cellSize/2
      yPos = fromIntegral y * cellSize - fromIntegral windowHeight/2 + cellSize/2
  in translate xPos yPos $ color col $ rectangleSolid cellSize cellSize

drawButton :: Bool -> Picture
drawButton running =
  let bx = -fromIntegral windowWidth/2 + 10
      by = fromIntegral windowHeight/2 - 60
      bw = 100
      bh = 50
      buttonColor = if running then makeColorI 200 0 0 255 else makeColorI 0 200 0 255
      rect = translate (bx + bw/2) (by + bh/2) $ color buttonColor $ rectangleSolid bw bh
      labelText = if running then "Stop" else "Start"
      label = translate (bx + 10) (by + bh/2 - 10) $ scale 0.15 0.15 $ color white $ text labelText
  in pictures [rect, label]

drawGeneration :: Int -> Picture
drawGeneration gen =
  let x = -fromIntegral windowWidth/2 + 10
      y = fromIntegral windowHeight/2 - 120
  in translate x y $ scale 0.15 0.15 $ color white $ text ("Gen: " ++ show gen)

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  initialWorld <- initialWorldIO
  let initialState = GameState { world = initialWorld, running = True, generation = 0 }
  play (InWindow "Colorful Cellular Automata" (windowWidth, windowHeight) (100,100))
       black
       fps
       initialState
       renderState
       handleEvent
       updateState


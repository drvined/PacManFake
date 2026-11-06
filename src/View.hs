-- | Turns the state of the application into a picture. Everything here is
--   pure, except 'viewIO', which only wraps 'renderApp' for 'playIO'.
module View (viewIO, renderApp, render, drawDeathAnimation) where

import Data.List (intercalate)
import Graphics.Gloss
import System.FilePath (takeBaseName)

import Config
import Maze
import Model

viewIO :: App -> IO Picture
viewIO = return . renderApp

renderApp :: App -> Picture
renderApp app = case screen app of
  LevelMenu message -> renderMenu (levelFiles app) (highScores app) message
  InGame gs         -> render gs

-- * The game screen

render :: GameState -> Picture
render gs = pictures [boardPicture gs, hud gs, overlay gs]

-- | The maze with everything on it, scaled to fit below the score bar.
boardPicture :: GameState -> Picture
boardPicture gs = translate 0 (-hudHeight / 2) (scale s s (translate x0 y0 board))
  where
    m     = maze gs
    w     = fromIntegral (mazeWidth m) * tileSize
    h     = fromIntegral (mazeHeight m) * tileSize
    s     = minimum [ maxBoardScale
                    , fromIntegral windowWidth / w
                    , (fromIntegral windowHeight - hudHeight) / h ]
    -- Tile (0,0) is drawn at the origin; shift so the maze is centred.
    x0    = -(w - tileSize) / 2
    y0    = (h - tileSize) / 2
    tick  = stepCount gs
    board = pictures ( drawMaze tick m
                     : map (draw tick) (items gs)
                    ++ characters
                    ++ map (draw tick) (deathAnimations gs) )
    -- Pac-Man and the ghosts are hidden while Pac-Man dies.
    characters
      | dying gs  = []
      | otherwise = map (draw tick) (ghosts gs) ++ [draw tick (player gs)]

drawMaze :: Int -> Maze -> Picture
drawMaze tick m = pictures [placeAt p (drawTile tick t) | (p, t) <- indexedTiles m]

-- | Move a picture drawn around the origin to the centre of a tile.
placeAt :: Pos -> Picture -> Picture
placeAt (Pos c r) = translate (fromIntegral c * tileSize) (negate (fromIntegral r * tileSize))

drawTile :: Int -> Tile -> Picture
drawTile _ Wall   = color wallColor (rectangleSolid tileSize tileSize)
drawTile _ Floor  = blank
drawTile _ Pellet = color pelletColor (circleSolid (radius * 0.2))
drawTile tick PowerPellet
  | even (tick `div` 2) = color pelletColor (circleSolid (radius * 0.55))
  | otherwise           = blank  -- power pellets blink

-- * Drawing the things on the board

-- | Things on the board that can draw themselves. 'shape' draws the thing
--   centred on the origin; the 'Int' is the step counter, used for simple
--   animations such as Pac-Man's mouth and flashing ghosts.
class HasPos a => Drawable a where
  shape :: Int -> a -> Picture

-- | Draw something on its own tile.
draw :: Drawable a => Int -> a -> Picture
draw tick x = placeAt (posOf x) (shape tick x)

instance Drawable Player where
  shape tick p = rotate (directionAngle (playerDir p)) (color pacmanColor (pacmanShape mouth))
    where mouth | even tick = mouthOpen
                | otherwise = mouthClosed

instance Drawable Ghost where
  shape tick g = pictures [color (ghostBodyColor tick g) ghostBody, ghostEyes (ghostDir g)]

instance Drawable Item where
  shape _ item = case itemKind item of
    Fruit -> cherry
    Candy -> candy

instance Drawable DeathAnimation where
  shape _ = drawDeathAnimation

-- | Pac-Man facing east, with a mouth opened @mouth@ degrees.
pacmanShape :: Float -> Picture
pacmanShape mouth = arcSolid (mouth / 2) (360 - mouth / 2) (radius * 0.85)

-- | Gloss rotates clockwise, starting from a picture that faces east.
directionAngle :: Direction -> Float
directionAngle East  = 0
directionAngle South = 90
directionAngle West  = 180
directionAngle North = 270

-- | Pac-Man folds up: his mouth opens until nothing is left, and then a few
--   sparks fly out. The animation spans many frames.
drawDeathAnimation :: DeathAnimation -> Picture
drawDeathAnimation a = color pacmanColor (pictures [body, sparks])
  where
    progress = 1 - timeLeft a / deathDuration          -- from 0 to 1
    mouth    = min 360 (mouthOpen + (360 - mouthOpen) * progress)
    body     = rotate (directionAngle North) (pacmanShape mouth)
    sparks
      | progress < sparkStart = blank
      | otherwise             = pictures (map spark [0 .. sparkCount - 1])
    spark i  = rotate (fromIntegral i * 360 / fromIntegral sparkCount)
                      (line [(radius * 0.3, 0), (radius * (0.3 + progress), 0)])

ghostBodyColor :: Int -> Ghost -> Color
ghostBodyColor tick g = case ghostMode g of
  Chasing -> ghostColor (ghostKind g)
  Evading t
    | t < evadeWarningTime && odd tick -> white  -- almost over: flash
    | otherwise                        -> evadingColor

ghostColor :: GhostKind -> Color
ghostColor Blinky = red
ghostColor Pinky  = makeColorI 255 184 255 255
ghostColor Inky   = cyan
ghostColor Clyde  = makeColorI 255 184 82 255

-- | Round head with a flat bottom.
ghostBody :: Picture
ghostBody = pictures
  [ translate 0 (radius * 0.1) (circleSolid (radius * 0.8))
  , translate 0 (-radius * 0.35) (rectangleSolid (radius * 1.6) (radius * 0.9))
  ]

-- | Two eyes looking in the direction the ghost moves.
ghostEyes :: Direction -> Picture
ghostEyes d = pictures (map eye [-1, 1])
  where
    (dx, dy) = screenVector d
    eye side = translate (side * radius * 0.32) (radius * 0.2) $ pictures
      [ color white (circleSolid (radius * 0.26))
      , translate (dx * radius * 0.12) (dy * radius * 0.12)
                  (color pupilColor (circleSolid (radius * 0.13)))
      ]

-- | A direction as a vector on the screen, where y points up.
screenVector :: Direction -> (Float, Float)
screenVector d = (fromIntegral dc, negate (fromIntegral dr))
  where (dc, dr) = directionVector d

cherry :: Picture
cherry = pictures
  [ color green (line [(-radius * 0.3, -radius * 0.1), (radius * 0.15, radius * 0.7)])
  , color green (line [( radius * 0.3, -radius * 0.2), (radius * 0.15, radius * 0.7)])
  , color red (translate (-radius * 0.3) (-radius * 0.3) (circleSolid (radius * 0.35)))
  , color red (translate ( radius * 0.3) (-radius * 0.4) (circleSolid (radius * 0.35)))
  ]

-- | A wrapped sweet: a round candy with a twisted wrapper on both sides.
candy :: Picture
candy = pictures
  [ color candyColor (polygon [(-radius * 0.3, 0), (-radius * 0.85, radius * 0.35), (-radius * 0.85, -radius * 0.35)])
  , color candyColor (polygon [( radius * 0.3, 0), ( radius * 0.85, radius * 0.35), ( radius * 0.85, -radius * 0.35)])
  , color candyColor (circleSolid (radius * 0.4))
  , color white (rectangleSolid (radius * 0.12) (radius * 0.7))
  ]

-- * Score bar and messages

-- | Score, lives and level name in three equal columns at the top.
hud :: GameState -> Picture
hud gs = translate (-halfWidth + margin) hudBaseline (pictures (zipWith placeColumn [0 ..] fields))
  where
    pac    = player gs
    fields = ["SCORE " ++ show (score pac), "LIVES " ++ show (lives pac), levelName gs]
    placeColumn i s = translate (i * hudColumnWidth) 0 (label bodySize white s)
    hudColumnWidth  = (fromIntegral windowWidth - 2 * margin) / fromIntegral (length fields)
    hudBaseline     = halfHeight - hudHeight / 2 - bodySize / 2

-- | A message on top of the board when the game is not running.
overlay :: GameState -> Picture
overlay gs = case status gs of
  Playing  -> blank
  Paused   -> banner "PAUSED" ["P: continue     M: menu"]
  GameOver -> banner "GAME OVER" [finalScore, "ENTER or M: back to the menu"]
  Won      -> banner "LEVEL CLEARED!" [finalScore, "ENTER or M: back to the menu"]
  where finalScore = "score: " ++ show (score (player gs))

-- | A dark box across the window with a big title and some lines below it.
--   The box is exactly as high as the text plus a margin above and below.
banner :: String -> [String] -> Picture
banner title rest = pictures
  [ color backdropColor (rectangleSolid (fromIntegral windowWidth) boxHeight)
  , translate 0 titleY (centredLabel headingSize yellow title)
  , translate 0 (titleY - lineHeight bodySize) (column bodySize (map (centredLabel bodySize white) rest))
  ]
  where
    boxHeight = lineHeight headingSize + fromIntegral (length rest) * lineHeight bodySize + 2 * margin
    titleY    = boxHeight / 2 - margin - headingSize

-- * The level menu

-- | The menu is laid out from the top down: every line is placed one line
--   height (of its own text size) below the previous one. The help text sits
--   at the bottom of the window, with the error message (if any) below it.
renderMenu :: [FilePath] -> [HighScore] -> Maybe String -> Picture
renderMenu files scores message = pictures
  [ translate 0 titleY (centredLabel titleSize yellow "PAC-MAN")
  , translate 0 taglineY (centredLabel smallSize candyColor "now with candy - better not eat it!")
  , translate left headingY (label bodySize white "Choose a level:")
  , translate left listY (column bodySize entries)
  , translate left helpY (column smallSize (map (label smallSize helpColor) help))
  , maybe blank (translate 0 messageY . centredLabel smallSize red) message
  ]
  where
    left     = -halfWidth + menuIndent
    titleY   = halfHeight - menuTopGap - titleSize
    taglineY = titleY - lineHeight smallSize
    headingY = taglineY - 2 * lineHeight bodySize   -- one empty line in between
    listY    = headingY - lineHeight bodySize
    messageY = -halfHeight + margin
    helpY    = messageY + fromIntegral (length help + 1) * lineHeight smallSize

    entries
      | null files = [label bodySize red ("no level files found in the folder " ++ levelsDirectory)]
      | otherwise  = zipWith entry levelKeys files
    entry key path = pictures
      [ label bodySize white (key : "     " ++ takeBaseName path)
      , translate bestScoreColumn 0 (label bodySize white (best (takeBaseName path)))
      ]
    best name = maybe "best: -" (("best: " ++) . show) (bestScore name scores)
    help =
      [ "arrow keys or WASD: move     P: pause     Esc: quit"
      , "pellet " ++ show pelletPoints ++ "   power pellet " ++ show powerPelletPoints
          ++ "   ghost " ++ show ghostPoints
      , "fruit " ++ show fruitPoints ++ "   candy " ++ show candyPoints
      , "fruit and candy appear after "
          ++ intercalate " and " (map show itemThresholds) ++ " pellets"
      ]

-- * Text helpers

-- | Pictures below each other, one line height apart; the first one stays
--   at the origin.
column :: Float -> [Picture] -> Picture
column size = pictures . zipWith below [0 ..]
  where below i = translate 0 (-i * lineHeight size)

-- | Distance between the baselines of two lines of text of a given size.
lineHeight :: Float -> Float
lineHeight size = size * lineSpacing

-- | Text whose left end of the baseline is at the origin, about @size@
--   pixels high. Gloss draws text about 100 pixels high, so we scale it down.
label :: Float -> Color -> String -> Picture
label size c = color c . scale k k . text
  where k = size / glyphHeight

-- | Text centred horizontally around the origin (approximately, since the
--   Gloss font does not have the same width for every letter).
centredLabel :: Float -> Color -> String -> Picture
centredLabel size c s = translate (-textWidth / 2) 0 (label size c s)
  where textWidth = fromIntegral (length s) * size * glyphAspect

-- * Sizes and colours

-- | Half the size of a tile; most shapes are measured relative to this.
radius :: Float
radius = tileSize / 2

halfWidth, halfHeight, margin :: Float
halfWidth  = fromIntegral windowWidth / 2
halfHeight = fromIntegral windowHeight / 2
margin     = 20

-- | Text sizes in pixels.
titleSize, headingSize, bodySize, smallSize :: Float
titleSize   = 50  -- the game title in the menu
headingSize = 30  -- the title of a banner (PAUSED, GAME OVER, ...)
bodySize    = 16  -- normal text: score bar, level list, banner lines
smallSize   = 12  -- tagline, help text, error messages

-- | Line height as a multiple of the text size.
lineSpacing :: Float
lineSpacing = 2

-- | Menu layout: space above the title, indent of the left-aligned text and
--   the x position of the "best" column in the level list.
menuTopGap, menuIndent, bestScoreColumn :: Float
menuTopGap      = 60
menuIndent      = 60
bestScoreColumn = 300

glyphHeight, glyphAspect :: Float
glyphHeight = 100   -- height of Gloss text before scaling
glyphAspect = 0.7   -- rough width / height of one character

mouthOpen, mouthClosed :: Float
mouthOpen   = 70
mouthClosed = 10

-- | The death animation shows sparks in its last part.
sparkStart :: Float
sparkStart = 0.75

sparkCount :: Int
sparkCount = 8

wallColor, pelletColor, pacmanColor, evadingColor, pupilColor, candyColor, helpColor, backdropColor :: Color
wallColor    = makeColorI 33 33 200 255
pelletColor  = makeColorI 255 220 180 255
pacmanColor  = yellow
evadingColor = makeColorI 60 60 255 255
pupilColor   = makeColorI 20 20 120 255
candyColor   = makeColorI 255 80 200 255
helpColor    = greyN 0.7
backdropColor = makeColor 0 0 0 0.8  -- black, slightly see-through

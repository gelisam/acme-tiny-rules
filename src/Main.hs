{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Codec.Picture ( readImage )
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Traversable (for)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (fromDynamicImage)


----------
-- Math --
----------

squared :: Float -> Float
squared x
  = x * x


------------
-- Images --
------------

imagePaths :: Map Int FilePath
imagePaths = Map.fromList
  [ (21, "images/21.png")
  , (29, "images/29.png")
  , (37, "images/37.png")
  , (47, "images/47.png")
  , (55, "images/55.png")
  , (71, "images/71.png")
  , (80, "images/80.png")
  , (95, "images/95.png")
  , (15, "images/15.png")
  , (115, "images/115.png")
  , (136, "images/136.png")
  , (149, "images/149.png")
  , (166, "images/166.png")
  , (179, "images/179.png")
  , (201, "images/201.png")
  , (225, "images/225.png")
  ]

type Images = Map Int Picture

loadImages :: IO Images
loadImages = do
  for imagePaths $ \path -> do
    readImage path >>= \case
      Left err -> fail err
      Right img -> case fromDynamicImage img of
        Nothing -> fail "Unsupported image format"
        Just pic -> pure pic

closestImage :: Images -> Int -> Picture
closestImage imgs key
  = case (below, above) of
      (Just (k1, pic1), Just (k2, pic2))
        | key - k1 < k2 - key
          -> pic1
        | otherwise
          -> pic2
      (Just (_, pic), _)
        -> pic
      (_, Just (_, pic))
        -> pic
      _ -> error "closestImage: empty Map"
  where
    below = Map.lookupLE key imgs
    above = Map.lookupGE key imgs


--------------------
-- Inference Rule --
--------------------

data Rule = Rule
  { rule_x0
      :: Float
  , rule_x1
      :: Float
  , rule_y
      :: Float
  }
  deriving Show

renderRule :: Images -> Rule -> Picture
renderRule imgs (Rule {..})
  = translate ((rule_x0 + rule_x1) / 2) rule_y
  $ ( translate 0 1
    $ closestImage imgs
    $ round
    $ abs (rule_x1 - rule_x0)
    )
 <> rectangleSolid (rule_x1 - rule_x0) 2

setX1 :: Float -> Rule -> Rule
setX1 x1 rule
  = rule { rule_x1 = x1 }

normalizeRule :: Rule -> Rule
normalizeRule (Rule x0 x1 y)
  | x0 <= x1 = Rule x0 x1 y
  | otherwise = Rule x1 x0 y

longEnough :: Rule -> Bool
longEnough (Rule x0 x1 _)
  = x1 - x0 > 3


------------
-- Eraser --
------------

eraserRadius :: Float
eraserRadius = 30.0

renderEraser :: Point -> Picture
renderEraser (x, y)
  = translate x y
  $ color (greyN 0.5)
  $ circleSolid eraserRadius

-- >        ...
-- >       | o |     oy
-- > ------'...'---- ry
-- > x0    p q p'  x1
eraseRule :: Point -> Rule -> [Rule]
eraseRule (qx, oy) rule@(Rule x0 x1 ry)
  | qoSquared >= poSquared || px >= x1 || px' <= x0
    = [rule]
  | otherwise
    = filter longEnough
      [ Rule x0 px ry
      , Rule px' x1 ry
      ]
  where
    qoSquared = squared (ry - oy)
    poSquared = squared eraserRadius
    pqSquared = poSquared - qoSquared
    pq = sqrt pqSquared
    px = qx - pq
    px' = qx + pq


-----------
-- World --
-----------

data World = World
  { world_rules
      :: [Rule]
  , world_currentRule
      :: Maybe Rule
  , world_eraser
      :: Maybe Point
  }
  deriving Show

renderWorld :: Images -> World -> Picture
renderWorld imgs (World {..}) = Pictures
  $ ( fmap (renderRule imgs)
    $ filter longEnough
    $ fmap normalizeRule
    $ toList world_currentRule
    )
 ++ map (renderRule imgs) world_rules
 ++ fmap renderEraser (toList world_eraser)

initialWorld :: World
initialWorld = World
  { world_rules
      = []
  , world_currentRule
      = Nothing
  , world_eraser
      = Nothing
  }

startNewRule :: Point -> World -> World
startNewRule (x, y) world
  = world
  { world_currentRule
      = Just
      $ Rule x x y
  }

continueNewRule :: Point -> World -> World
continueNewRule (x, _) world@(World {..})
  = world
  { world_currentRule
      = fmap (setX1 x) world_currentRule
  }

completeNewRule :: World -> World
completeNewRule world@(World {..})
  = world
  { world_rules
      = ( filter longEnough
        $ fmap normalizeRule
        $ toList world_currentRule
        )
     ++ world_rules
  , world_currentRule
      = Nothing
  }

moveEraser :: Point -> World -> World
moveEraser pt world@(World {..})
  = world
  { world_rules
      = concatMap (eraseRule pt) world_rules
  , world_eraser
      = Just pt
  }

stopErasing :: World -> World
stopErasing world
  = world
  { world_eraser
      = Nothing
  }


--------------
-- Handlers --
--------------

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ pt) world
  = startNewRule pt world
handleEvent (EventKey (MouseButton LeftButton) Up _ _) world
  = completeNewRule world
handleEvent (EventKey (MouseButton RightButton) Down _ pt) world
  = moveEraser pt world
handleEvent (EventKey (MouseButton RightButton) Up _ _) world
  = stopErasing world
handleEvent (EventMotion pt) world@(World {..})
  | isJust world_currentRule
    = continueNewRule pt world
  | isJust world_eraser
    = moveEraser pt world
  | otherwise
    = world
handleEvent _ world
  = world

handleTick :: Float -> World -> World
handleTick _ world = world


----------
-- Main --
----------

main :: IO ()
main = do
  putStrLn "Loading images..."
  imgs <- loadImages
  putStrLn "Images loaded."

  play
    (InWindow "Tiny Rules" (500, 500) (10, 10))
    white
    30
    initialWorld
    (renderWorld imgs)
    handleEvent
    handleTick


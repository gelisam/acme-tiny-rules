{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Codec.Picture ( readImage )
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (fromDynamicImage)


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

-----------
-- State --
-----------

data Rule = Rule
  { rule_x0
      :: Float
  , rule_x1
      :: Float
  , rule_y
      :: Float
  }

setX1 :: Float -> Rule -> Rule
setX1 x1 rule
  = rule { rule_x1 = x1 }

data World = World
  { world_rules
      :: [Rule]
  , world_currentRule
      :: Maybe Rule
  }

initialWorld :: World
initialWorld = World
  { world_rules = []
  , world_currentRule = Nothing
  }


--------------
-- Handlers --
--------------

handleEvent :: Event -> World -> World
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) world
  = world
  { world_currentRule
      = Just
      $ Rule x x y
  }
handleEvent (EventKey (MouseButton LeftButton) Up _ (_, _)) world@(World {..})
  = world
  { world_rules
      = toList world_currentRule
     ++ world_rules
  , world_currentRule
      = Nothing
  }
handleEvent (EventMotion (x, _)) world@(World {..})
  = world
  { world_currentRule
      = fmap (setX1 x) world_currentRule
  }
handleEvent _ world
  = world

handleTick :: Float -> World -> World
handleTick _ world = world


-------------
-- Display --
-------------

renderRule :: Images -> Rule -> Picture
renderRule imgs (Rule {..})
  = translate ((rule_x0 + rule_x1) / 2) rule_y
  $ ( translate 0 1
    $ closestImage imgs
    $ round (rule_x1 - rule_x0)
    )
 <> rectangleSolid (rule_x1 - rule_x0) 2

renderWorld :: Images -> World -> Picture
renderWorld imgs (World {..}) = Pictures
  $ fmap (renderRule imgs) (toList world_currentRule)
 ++ map (renderRule imgs) world_rules


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


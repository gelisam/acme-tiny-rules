{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Codec.Picture ( readImage )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (fromDynamicImage)


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



main :: IO ()
main = do
  putStrLn "Loading images..."
  pics <- for imagePaths $ \path -> do
    readImage path >>= \case
      Left err -> fail err
      Right img -> case fromDynamicImage img of
        Nothing -> fail "Unsupported image format"
        Just pic -> pure pic

  let window = InWindow "acme-tiny-rules" (800, 600) (10, 10)
  display window white (pics Map.! 15)

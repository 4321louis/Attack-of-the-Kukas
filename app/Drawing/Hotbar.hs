-- Hotbar visual components

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Drawing.Hotbar where

import Apecs
import Apecs.Gloss
import Apecs.Extension

import Plant.Seed
import Drawing.Camera
import Drawing.Sprites

----- Hotbar

allSeeds :: [Seed]
allSeeds = [GreenSeed, RedSeed, BlueSeed, Spore]

buttonInfo :: Seed -> (String, Picture)
buttonInfo GreenSeed = ("Q", greenSeed)
buttonInfo RedSeed = ("W", redSeed)
buttonInfo BlueSeed = ("E", blueSeed)
buttonInfo Spore = ("R", spore)

button :: Seed -> Int -> Picture
button seed count = spritePic <> keyText <> countText
    where   (key, sprite) = buttonInfo seed
            spritePic = scale 1.4 1.4 $ sprite
            keyText = translate (-33.0) 3.0 . scale 0.15 0.15 . color white . Text $ key
            countText = translate 33.0 (-7.0) . scale 0.2 0.2 . color white . Text $ show count

-- Draw all and translate buttons
drawButtons :: [Picture] -> Picture
drawButtons [b] = b
drawButtons (b:bs) = translate 0 (fromIntegral $ buttonSpacing * num) b <> drawButtons bs
    where   num = length bs
            buttonSpacing = 70

drawHotbar :: [Int] -> Picture
drawHotbar inv = drawButtons buttons
    where buttons = map (\(count, seed) -> button seed count) $ zip inv allSeeds

----- Crafting


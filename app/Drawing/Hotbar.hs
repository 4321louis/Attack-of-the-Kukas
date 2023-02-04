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
import Plant.Plant
import Drawing.Camera
import Drawing.Sprites

import Debug.Trace (trace)

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

drawCraft craft@[s1, s2] = seed'' <> seed' <> plant'
    where   plant = getPlantSprite $ getPlant craft
            seed = getSeedSprite s1
            plant' = scale 1.2 1.2 $ plantCircle plant
            seed' = translate fromPlantCircle (fromPlantCircle + offset) $ seedCircle seed
            seed'' = translate (fromPlantCircle + offset) fromPlantCircle $ seedCircle seed
            fromPlantCircle = -120
            offset = 65


-- todo: make colour
plantCircle s = circle <> sprite
    where   circle = color white $ circleSolid 70
            sprite = translate 1 (-20.0) s

seedCircle s = circle <> sprite
    where   circle = color white $ circleSolid 35
            sprite = scale 1.2 1.2 $ s
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
import Player

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
            keyText = translate (-33.0) 3.0 . scale 0.08 0.08 . color white . Text $ key
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

drawCraft :: [Int] -> [Seed] -> Picture
drawCraft inv craft@[s1, s2] = seed' <> seed <> plant' <> plantName
    where   -- Sprite handling
            plantSprite = getPlantSprite $ getPlant craft
            [seedSprite, seedSprite'] = map getSeedSprite craft
            
            -- Inv checking
            -- when same seeds are selected
            combinedInv = if s1 == s2 then all (>=0) $ updateInv inv craft else True 
            seedInv = combinedInv && (all (>=0) $ updateInv inv [s1])
            seedInv' = all (>=0) $ updateInv inv [s2]
            
            plantInv = seedInv && seedInv'

            -- Drawing Crafting circles
            plant' = scale 1.2 1.2 $ plantCircle plantSprite plantInv
            seed = translate (fromPlantCircle + offset) fromPlantCircle $ seedCircle seedSprite seedInv
            seed' = translate fromPlantCircle (fromPlantCircle + offset) $ seedCircle seedSprite' seedInv'
            
            plantName = translate 20 (-130) $ scale 0.15 0.15 $ color white $ Text (show $ getPlant craft) 
            fromPlantCircle = -120
            offset = 65

invalidColour :: Color
invalidColour = makeColor 1 0 0 0.8

plantInvalidColour :: Color
plantInvalidColour = makeColor 0.25 0.08 0.08 0.6

plantCircle :: Picture -> Bool -> Picture
plantCircle s invCheck = circle <> sprite <> invWarning
    where   circleSize = 70
            circle = color white $ circleSolid circleSize
            sprite = translate 1 (-20.0) s
            invWarning = if invCheck then Blank else color plantInvalidColour $ circleSolid circleSize

seedCircle :: Picture -> Bool -> Picture
seedCircle s invCheck = circle <> invWarning <> sprite 
    where   circleSize = 35
            circle = color white $ circleSolid circleSize
            sprite = scale 1.2 1.2 $ s
            invWarningCircle = color invalidColour $ circleSolid circleSize
            invWarning = if invCheck then Blank else invWarningCircle
-- handling sprites

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

module Drawing.Sprites where

import Apecs
import Apecs.Gloss
import Control.Monad
import Graphics.Gloss.Game (jpg,png)
import System.Random
import Linear (V2(..))
import Apecs.Extension

import Worlds

newtype Sprite = Sprite Picture deriving (Show)
instance Component Sprite where type Storage Sprite = Map Sprite

data AnimatedSprite = AnimatedSprite Float [Picture] deriving (Show)
instance Component AnimatedSprite where type Storage AnimatedSprite = Map AnimatedSprite

newtype Particle = Particle Float deriving (Show)
instance Component Particle where type Storage Particle = Map Particle

spriteDir :: String
spriteDir = "./assets/Sprites/"

triangle, diamond, testPicture1, testPicture2, testPicture3, testPicture4, playerSprite, targetSprite1, stickFigure, targetSprite2, targetSprite3, targetSprite4, bulletSprite :: Picture
triangle = Line [(0, 0.5), (-0.5, -0.5), (0.5, -0.5), (0, 0.5)]
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]
stickFigure = Line [(0,0),(0,-1),(-0.7,-1.3)] <> Line [(0.7,-1.3),(0,-1),(0,-1.5),(-0.7,-2)] <> Line [(0,-1.5),(0.7,-2)]
testPicture1 = scale 0.1 0.1 . jpg $ spriteDir ++ "Test/test_picture.jpg"
testPicture2 = scale 0.1 0.1 . png $ spriteDir ++ "Test/test_picture2.png"
testPicture3 = scale 0.4 0.4 . png $ spriteDir ++ "Test/test_picture3.png"
testPicture4 = scale 0.5 0.5 . png $ spriteDir ++ "Test/test_picture4.png"

cactus :: Picture
cactus = translate 0 9.5 . png $ spriteDir ++ "Entities/BRCactus.png"
enchanter = translate 0 9.5 . png $ spriteDir ++ "Test/HealingTemp.png"

playerSprite = rotate 90 . color white . scale 10 20 $ triangle
targetSprite1 = rotate 90 $ testPicture1 <> (translate 0 (negate 20) . scale 10 10 . color white $ stickFigure)
targetSprite2 = testPicture2 <> (translate 0 (negate 20) . scale 10 10 . color white $ stickFigure)
targetSprite3 = testPicture3 <> (translate 0 (negate 20) . scale 10 10 . color white $ stickFigure)
targetSprite4 = testPicture4 <> (translate 0 (negate 20) . scale 10 10 . color white $ stickFigure)
bulletSprite = color yellow . scale 4 4 $ diamond

animTargetSprite1, animTargetSprite2 :: AnimatedSprite
animTargetSprite1 = AnimatedSprite 0.6 [targetSprite4,targetSprite3,targetSprite2,targetSprite1]
animTargetSprite2 = AnimatedSprite 0.6 [targetSprite1,targetSprite2,targetSprite3,targetSprite4]

animatedSprites :: (HasMany w [Time, AnimatedSprite, Sprite]) => Float -> System w ()
animatedSprites dT = cmap $ \(AnimatedSprite rate frames, Time t) -> Sprite (frames !! mod (floor ((t + dT) / rate)) (length frames))


spawnParticles ::  (HasMany w [Position, Velocity, Particle, EntityCounter]) => Int -> Position -> (Float, Float) -> (Float, Float) -> System w ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
    vx <- liftIO $ randomRIO dvx
    vy <- liftIO $ randomRIO dvy
    t <- liftIO $ randomRIO (0.02, 0.3)
    newEntity (Particle t, pos, Velocity (V2 vx vy))

stepParticles :: (HasMany w [Position, Velocity, Particle]) => Float -> System w ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Kinetic)
        else Left $ Particle (t - dT)
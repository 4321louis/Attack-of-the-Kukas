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

cactus, enchanter, rockPlant , seedSeeker, aoeMushroom , attackSpeedFlower, vampireFlower, birdOfParadise, mycelium:: Picture
cactus = translate 0 9.5 . png $ spriteDir ++ "Entities/BRCactus.png"
enchanter = translate 0 6 . png $ spriteDir ++ "Entities/GBShield.png"
rockPlant = png $ spriteDir ++ "Entities/BBRock.png"
seedSeeker = translate 0 11.5 . png $ spriteDir ++ "Entities/GGSeedSeeker.png"
aoeMushroom = translate 0 (-3) . png $ spriteDir ++ "Entities/BSAoE.png"
attackSpeedFlower = translate 0 (-4) . png $ spriteDir ++ "Entities/GRAttackSpeed.png"
vampireFlower = png $ spriteDir ++ "Entities/GSVampire.png"
birdOfParadise = translate 0 (-7) . png $ spriteDir ++ "Entities/RRLazer.png"
mycelium = translate 0 1.5 . png $ spriteDir ++ "Entities/RSDoT.png"

shieldEffect, attackSpeedEffect :: Picture
shieldEffect = scale 1.5 1.5 . png $ spriteDir ++ "Effects/ShieldEffect.png"
attackSpeedEffect = scale 1.5 1.5 . png $ spriteDir ++ "Effects/AttackSpeedEffect.png"

redSeed, greenSeed, blueSeed, spore :: Picture
redSeed =  scale 0.6 0.6 . png $ spriteDir ++ "UI/RSeed.png"
greenSeed = scale 0.6 0.6 . png $ spriteDir ++ "UI/GSeed.png"
blueSeed =  scale 0.6 0.6 . png $ spriteDir ++ "UI/BSeed.png"
spore =  scale 0.6 0.6 . png $ spriteDir ++ "UI/FungalSpore.png"

dronekukasf1, dronekukasf2, dronekukasf3, dronekukasf4, dronekukasf5 :: Picture
dronekukasf1 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump1.png"
dronekukasf2 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump2.png"
dronekukasf3 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump3.png"
dronekukasf4 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump4.png"
dronekukasf5 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump5.png"

droneKukasWalkLeft, droneKukasWalkRight:: AnimatedSprite
droneKukasWalkRight = AnimatedSprite 0.15 [dronekukasf1,dronekukasf2,dronekukasf3,dronekukasf4,dronekukasf5]
droneKukasWalkLeft = AnimatedSprite 0.15 $ map (scale (-1) 1) [dronekukasf1,dronekukasf2,dronekukasf3,dronekukasf4,dronekukasf5]

aoe1, aoe2, aoe3, aoe4, aoe5, aoe6, aoe7, aoe8 :: Picture
aoe1 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect1.png"
aoe2 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect2.png"
aoe3 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect3.png"
aoe4 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect4.png"
aoe5 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect5.png"
aoe6 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect6.png"
aoe7 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect7.png"
aoe8 = png $ spriteDir ++ "Effects/AoEEffect/AoEEffect7.png"

aoeEffect, aoeEffectMini :: AnimatedSprite
aoeEffect = AnimatedSprite 0.25 $ map (scale 22 22) [aoe1, aoe2, aoe3, aoe4, aoe5, aoe6, aoe7, aoe8]
aoeEffectMini = AnimatedSprite 0.25 $ map (scale 1 1) [aoe1, aoe2, aoe3, aoe4, aoe5, aoe6, aoe7, aoe8]
 
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


-- spawnParticles ::  (HasMany w [Position, Velocity, Particle, EntityCounter]) => Int -> Position -> (Float, Float) -> (Float, Float) -> System w ()
-- spawnParticles n pos dvx dvy = replicateM_ n $ do
--     vx <- liftIO $ randomRIO dvx
--     vy <- liftIO $ randomRIO dvy
--     t <- liftIO $ randomRIO (0.02, 0.3)
--     newEntity (Particle t, pos, Velocity (V2 vx vy))

stepParticles :: (HasMany w [Position, Particle, Sprite]) => Float -> System w ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Position, Sprite)
        else Left  $ Particle (t-dT)

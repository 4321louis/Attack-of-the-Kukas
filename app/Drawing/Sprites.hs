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

data AnimatedSprite = AnimatedSprite Float [Picture] deriving (Show,Eq)
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

cactus, enchanter, rockPlant , seedSeeker, aoeMushroom , attackSpeedFlower, vampireFlower, birdOfParadise, mycelium, necromancer:: Picture
cactus = translate 0 9.5 . png $ spriteDir ++ "Entities/BRCactus.png"
enchanter = translate 0 6 . png $ spriteDir ++ "Entities/GBShield.png"
rockPlant = png $ spriteDir ++ "Entities/BBRock.png"
seedSeeker = translate 0 11.5 . png $ spriteDir ++ "Entities/GGSeedSeeker.png"
aoeMushroom = translate 0 (-3) . png $ spriteDir ++ "Entities/BSAoE.png"
attackSpeedFlower = translate 0 (-4) . png $ spriteDir ++ "Entities/GRAttackSpeed.png"
vampireFlower = png $ spriteDir ++ "Entities/GSVampire.png"
birdOfParadise = translate 0 (-7) . png $ spriteDir ++ "Entities/RRLazer.png"
mycelium = translate 0 1.5 . png $ spriteDir ++ "Entities/RSDoT.png"
necromancer = translate 0 3 . png $ spriteDir ++ "Entities/SSNecro.png"

shieldEffect, attackSpeedEffect :: Picture
shieldEffect = scale 1.5 1.5 . png $ spriteDir ++ "Effects/ShieldEffect.png"
attackSpeedEffect = scale 1.5 1.5 . png $ spriteDir ++ "Effects/AttackSpeedEffect.png"

redSeed, greenSeed, blueSeed, spore :: Picture
redSeed =  scale 0.6 0.6 . png $ spriteDir ++ "UI/RSeed.png"
greenSeed = scale 0.6 0.6 . png $ spriteDir ++ "UI/GSeed.png"
blueSeed =  scale 0.6 0.6 . png $ spriteDir ++ "UI/BSeed.png"
spore =  scale 0.6 0.6 . png $ spriteDir ++ "UI/FungalSpore.png"

dronekukaswf1, dronekukaswf2, dronekukaswf3, dronekukaswf4, dronekukaswf5 :: Picture
dronekukaswf1 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump1.png"
dronekukaswf2 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump2.png"
dronekukaswf3 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump3.png"
dronekukaswf4 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump4.png"
dronekukaswf5 = png $ spriteDir ++ "Entities/Kukas/DroneKukasJump5.png"
dronekukasaf1 = png $ spriteDir ++ "Entities/Kukas/DroneKukasAttack1.png"
dronekukasaf2 = png $ spriteDir ++ "Entities/Kukas/DroneKukasAttack2.png"
dronekukasaf3 = png $ spriteDir ++ "Entities/Kukas/DroneKukasAttack3.png"
dronekukasaf4 = png $ spriteDir ++ "Entities/Kukas/DroneKukasAttack4.png"
dronekukasaf5 = png $ spriteDir ++ "Entities/Kukas/DroneKukasAttack5.png"

droneKukasWalkLeft, droneKukasWalkRight:: AnimatedSprite
droneKukasWalkRight = AnimatedSprite 0.15 [dronekukaswf1,dronekukaswf2,dronekukaswf3,dronekukaswf4,dronekukaswf5]
droneKukasWalkLeft = AnimatedSprite 0.15 $ map (scale (-1) 1) [dronekukaswf1,dronekukaswf2,dronekukaswf3,dronekukaswf4,dronekukaswf5]
droneKukasAttackLeft, droneKukasAttackRight:: AnimatedSprite
droneKukasAttackRight = AnimatedSprite 0.15 [dronekukasaf1,dronekukasaf2,dronekukasaf3,dronekukasaf4,dronekukasaf5]
droneKukasAttackLeft = AnimatedSprite 0.15 $ map (scale (-1) 1) [dronekukasaf1,dronekukasaf2,dronekukasaf3,dronekukasaf4,dronekukasaf5]

aoe1, aoe2, aoe3, aoe4, aoe5, aoe6, aoe7, aoe8 :: Picture
aoe1 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG1.png"
aoe2 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG2.png"
aoe3 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG3.png"
aoe4 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG4.png"
aoe5 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG5.png"
aoe6 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG6.png"
aoe7 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG7.png"
aoe8 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG7.png"

aoeb1, aoeb2, aoeb3, aoeb4, aoeb5, aoeb6, aoeb7, aoeb8 :: Picture
aoeb1 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG1.png"
aoeb2 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG2.png"
aoeb3 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG3.png"
aoeb4 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG4.png"
aoeb5 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG5.png"
aoeb6 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG6.png"
aoeb7 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG7.png"
aoeb8 = png $ spriteDir ++ "Effects/AoEEffectBIG/AoEEffectBIG8.png"

dot1, dot2, dot3, dot4 :: Picture
dot1 = png $ spriteDir ++ "Effects/DoTEffect/DoTEffect1.png"
dot2 = png $ spriteDir ++ "Effects/DoTEffect/DoTEffect2.png"
dot3 = png $ spriteDir ++ "Effects/DoTEffect/DoTEffect3.png"
dot4 = png $ spriteDir ++ "Effects/DoTEffect/DoTEffect4.png"

dotEffect ,aoeEffect, aoeEffectMini :: AnimatedSprite
aoeEffect = AnimatedSprite 0.25 $ map (scale 1 1) [aoeb1, aoeb2, aoeb3, aoeb4, aoeb5, aoeb6, aoeb7, aoeb8]
aoeEffectMini = AnimatedSprite 0.25 $ map (scale 4 4) [aoe1, aoe2, aoe3, aoe4, aoe5, aoe6]
dotEffect = AnimatedSprite 0.1 $ map (scale 2 2) [dot1, dot2, dot3, dot4]

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

victoryBg, gameOverBg :: Picture
victoryBg = png $ spriteDir ++ "Background/victory.png"
gameOverBg = png $ spriteDir ++ "Background/gameover.png"

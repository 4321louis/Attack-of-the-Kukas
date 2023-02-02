-- FUCK I LOVE PLANTS :D

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

module Plant.Plant where
    
import Drawing.Sprites
import Apecs
import Apecs.Gloss
import Apecs.Extension
import Control.Monad
import Misc
import Graphics.Gloss.Game (jpg,png)
import System.Random
import qualified Linear as L

import Enemy.Enemy
import Worlds
import Plant.Seed
import Linear (V2(..))
import Structure.Structure

data Plant = Plant deriving (Show)
instance Component Plant where type Storage Plant = Map Plant

data Cactus = Cactus deriving (Show)
instance Component Cactus where type Storage Cactus = Map Cactus

data Enchanter = Enchanter deriving (Show)
instance Component Enchanter where type Storage Enchanter = Map Enchanter

data RockPlant = RockPlant deriving (Show)
instance Component RockPlant where type Storage RockPlant = Map RockPlant

data SeedSeeker = SeedSeeker deriving (Show)
instance Component SeedSeeker where type Storage SeedSeeker = Map SeedSeeker

cactusDmg, enchanterShield :: Float
cactusDmg = 20
enchanterShield = 5

doPlants :: (HasMany w [Enemy, Position, Plant, Score, Enchanter, Cactus, SeedSeeker, Time, Hp, EntityCounter, Sprite, Seed]) => Float -> System w ()
doPlants dT = do
    doCactusAttack dT
    doEnchanting dT
    doSeedSeeking dT
    --do GB - Healer
    --do GR - Attackspeed
    --do GS - Vampiric 
    --do BB - ROCK PLANT
    --do BR - Cactus
    --do BS - Aoe mushroom, corpses fume as well
    --do RR - Lazer/swatter
    --do RS - Damage over time
    --do SS - Necromancy

-- doCactusAttack :: (HasMany w [Enemy, Position, Plant]) => System w ()
-- doCactusAttack =
--     cmap $ \(Plant, Position posP, etyP) ->
--         cfold (\b (Enemy e, Position posE)-> b || L.norm (posE - posP) < plantRange ) False
--             modify global $ \(Score x) -> Score (x + hitBonus)

newCactus :: (HasMany w [Cactus, Plant, Position, Hp, Sprite, Structure, EntityCounter]) => (Float, Float) -> System w Entity
newCactus (x, y) = newEntity (Cactus, Plant, Position (V2 x y), Hp 20 20 0, Sprite cactus)
newEnchanter :: (HasMany w [Enchanter, Plant, Position, Hp, Sprite, Structure, EntityCounter]) => (Float, Float) -> System w Entity
newEnchanter (x, y) = newEntity (Enchanter, Plant, Position (V2 x y), Hp 4 4 0, Sprite enchanter, Structure [(x+64,y),(x-64,y),(x,y+64),(x,y-64)])
newSeedSeeker :: (HasMany w [SeedSeeker, Plant, Position, Hp, Sprite, Structure, EntityCounter]) => (Float, Float) -> System w Entity
newSeedSeeker (x, y) = newEntity (SeedSeeker, Plant, Position (V2 x y), Hp 20 20 0, Sprite seedSeeker, Structure [(x+64,y),(x-64,y),(x,y+64),(x,y-64)])
newRockPlant :: (HasMany w [RockPlant, Plant, Position, Hp, Sprite, Structure, EntityCounter]) => (Float, Float) -> System w Entity
newRockPlant (x, y) = newEntity (RockPlant, Plant, Position (V2 x y), Hp 80 80 0, Sprite rockPlant, Structure [(x+64,y),(x-64,y),(x,y+64),(x,y-64)])

doCactusAttack :: (HasMany w [Enemy, Position, Cactus, Score, Time, Hp]) => Float -> System w ()
doCactusAttack dT =
    cmapM_ $ \(Cactus, Position posP) -> do
        cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 0) $
            triggerEvery dT 1 0.6 (modify etyE $ \(Enemy _ _, hp) -> dealDamage hp cactusDmg)

doEnchanting :: (HasMany w [Enchanter, Plant, Position, Time, Hp]) => Float -> System w ()
doEnchanting dT = 
    cmapM_ $ \(Enchanter, Position posEch) -> do
        cmapM_ $ \(Plant, Position posP, etyP) -> when (L.norm (posEch - posP) < tileRange 1) $
            triggerEvery dT 1 0.6 (modify etyP $ \(Plant, hp) -> shieldHp hp enchanterShield)

doSeedSeeking :: (HasMany w [SeedSeeker, Seed, Sprite, Plant, Position, Time, EntityCounter]) => Float -> System w ()
doSeedSeeking dT = 
    cmapM_ $ \(SeedSeeker, Position (V2 x y)) -> do
        seed <- liftIO $ randomRIO (0,3)
        xoff <- liftIO $ randomRIO (-32,32)
        yoff <- liftIO $ randomRIO (-32,-1)
        triggerEvery dT 30 0.6 $ newEntity ([GreenSeed,RedSeed,BlueSeed,Spore]!! seed, Position (V2 xoff yoff + V2 x y), Sprite $ [greenSeed,redSeed,blueSeed,spore] !! seed )


-- doCactusAttack :: (HasMany w [Enemy, Position, Plant, Score]) => System w ()
-- doCactusAttack =
--     cmapM_ $ \(Plant, Position posP) -> do
--         inRange <- cfold (\b (Enemy _ _ _, Position posE) -> b || L.norm (posE - posP) < plantRange ) False
--         when (inRange) $ modify global $ \(Score x) -> Score (x + 30)

-- handleCollisions :: SystemW ()
-- handleCollisions =
--     cmapM_ $ \(Target, Position posT, etyT) ->
--         cmapM_ $ \(Bullet, Position posB, etyB) ->
--             when (L.norm (posT - posB) < 10) $ do
--                 destroy etyT (Proxy @(Target, Kinetic))
--                 destroy etyB (Proxy @(Bullet, Kinetic))
--                 spawnParticles 15 (Position posB) (-500, 500) (200, -50)
--                 modify global $ \(Score x) -> Score (x + hitBonus)
--                 modify global $ \(Camera pos cScale) -> Camera pos (0.85*cScale)
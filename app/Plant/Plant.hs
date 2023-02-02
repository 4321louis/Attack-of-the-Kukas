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

data Plant = Plant deriving (Show)
instance Component Plant where type Storage Plant = Map Plant

data Cactus = Cactus deriving (Show)
instance Component Cactus where type Storage Cactus = Map Cactus

data Enchanter = Enchanter deriving (Show)
instance Component Enchanter where type Storage Enchanter = Map Enchanter

data RockPlant = RockPlant deriving (Show)
instance Component RockPlant where type Storage RockPlant = Map RockPlant

cactusDmg :: Float
cactusDmg = 20

doPlants :: (HasMany w [Enemy, Position, Plant, Score, Cactus, Time, Hp]) => Float -> System w ()
doPlants dT = do
    doCactusAttack dT
    --do GG - SeedSeeker
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

doCactusAttack :: (HasMany w [Enemy, Position, Cactus, Score, Time, Hp]) => Float -> System w ()
doCactusAttack dT =
    cmapM_ $ \(Cactus, Position posP) -> do
        cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 0) $
            triggerEvery dT 1 0.6 (modify etyE $ \(Enemy _ _, hp) -> dealDamage hp cactusDmg)

{- doEnchanting :: (HasMany w [Enchanter, Position, Time]) => Float -> System w ()
doEnchanting dT = 
    cmapM_ $ \(Enchanter, Position posEch) -> do
        cmapM_ $ \(Enchanter, Position posEnch, etyEnch) -> when (L.norm (posEch - posEnch) < tileRange 1) -}


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
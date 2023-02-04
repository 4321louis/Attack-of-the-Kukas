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
{-# LANGUAGE BlockArguments #-}

module Plant.Plant where

import Drawing.Sprites
import Apecs
import Apecs.Extension
<<<<<<< HEAD
import Apecs.Gloss 
=======
import Apecs.Gloss
>>>>>>> main
import Control.Monad
import Misc
import System.Random
import qualified Linear as L

import Audio
import Enemy.Enemy
import Enemy.Pathfinding
import Worlds
import Plant.Seed
import Linear (V2(..))
import Structure.Structure
import GHC.IO.Encoding.Types (EncodeBuffer)

type AllPlantComps = (Position, Structure, Sprite, Hp, Plant)

data Plant = RockPlant | Cactus | BigMushroom | Enchanter | SeedSeeker | CorpseFlower | VampireFlower | BirdOfParadise | Mycelium | Necromancer deriving (Show)
instance Component Plant where type Storage Plant = Map Plant

bigMushroomDmg, cactusDmg, enchanterShield :: Float
bigMushroomDmg = 20
cactusDmg = 20
lazerDmg = 100
enchanterShield = 5

-- doCactusAttack :: (HasMany w [Enemy, Position, Plant]) => System w ()
-- doCactusAttack =
--     cmap $ \(Plant, Position posP, etyP) ->
--         cfold (\b (Enemy e, Position posE)-> b || L.norm (posE - posP) < plantRange ) False
--             modify global $ \(Score x) -> Score (x + hitBonus)

getPlant :: [Seed] -> Plant
getPlant [GreenSeed, GreenSeed] = SeedSeeker
getPlant [GreenSeed, BlueSeed] = Enchanter
getPlant [GreenSeed, RedSeed] = CorpseFlower
getPlant [GreenSeed, Spore] = VampireFlower
getPlant [BlueSeed, BlueSeed] = RockPlant
getPlant [BlueSeed, RedSeed] = Cactus
getPlant [BlueSeed, Spore] = BigMushroom
getPlant [RedSeed, RedSeed] = BirdOfParadise
getPlant [RedSeed, Spore] = Mycelium
getPlant [Spore, Spore] = Necromancer
getPlant [x, y] = getPlant [y, x]
getPlant _ = SeedSeeker

getPlantSprite :: Plant -> Picture
getPlantSprite Cactus = cactus
getPlantSprite Enchanter = enchanter
getPlantSprite SeedSeeker = seedSeeker
getPlantSprite RockPlant = rockPlant
getPlantSprite CorpseFlower = attackSpeedFlower
getPlantSprite VampireFlower = vampireFlower
getPlantSprite BigMushroom = aoeMushroom
getPlantSprite BirdOfParadise = birdOfParadise
getPlantSprite Mycelium = mycelium

getSprite _ = seedSeeker

newPlant :: (HasMany w [Plant, Position, Hp, Sprite, Structure, EntityCounter]) => Plant -> V2 Float -> System w Entity
newPlant Cactus pos = newEntity (Cactus, Position pos, Hp 20 20 0, Sprite cactus)
newPlant Enchanter pos = newEntity (Enchanter, Position pos, Hp 4 4 0, Sprite enchanter, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant SeedSeeker pos = newEntity (SeedSeeker, Position pos, Hp 20 20 0, Sprite seedSeeker, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant RockPlant pos = newEntity (RockPlant, Position pos, Hp 80 80 0, Sprite rockPlant, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant CorpseFlower pos = newEntity (CorpseFlower, Position pos, Hp 20 20 0, Sprite attackSpeedFlower, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant VampireFlower pos = newEntity (VampireFlower, Position pos, Hp 20 20 0, Sprite vampireFlower, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant BigMushroom pos = newEntity (BigMushroom, Position pos, Hp 20 20 0, Sprite aoeMushroom, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant BirdOfParadise pos = newEntity (BirdOfParadise, Position pos, Hp 20 20 0, Sprite birdOfParadise, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant Mycelium pos = newEntity (Mycelium, Position pos, Hp 20 20 0, Sprite mycelium, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant Necromancer pos = newEntity (Necromancer, Position pos, Hp 20 20 0, Sprite mycelium, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))

doPlants :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, Seed, Particle, AnimatedSprite])=> Float -> System w ()
doPlants dT = do 
    doAttacks dT
    doOnDeaths

doAttacks :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, Seed, Particle, AnimatedSprite])=> Float -> System w ()
doAttacks dT = cmapM_ $ \(plant::Plant, Position pos, ety) ->
    case plant of
        Cactus -> doCactusAttack dT pos
        Enchanter -> doEnchanting dT pos
        SeedSeeker -> doSeedSeeking dT pos
        BigMushroom -> doBigMushroomAttack dT pos
        BirdOfParadise -> doLazerAttack dT pos
        RockPlant -> doRockPlant dT ety
        --do GR - Attackspeed
        --do GS - Vampiric 
        --do RS - Damage over time
        --do SS - Necromancy
        _ -> do return ()

doOnDeaths :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, Seed, Particle]) => System w ()
doOnDeaths = return ()

doRockPlant :: (HasMany w [Time, Hp]) => Float -> Entity -> System w ()
doRockPlant dT ety = triggerEvery dT 1 0.6 $ modify ety $ (`healHp` 1)


doCactusAttack :: (HasMany w [Enemy, Position, Plant, Time, Hp]) => Float -> V2 Float -> System w ()
doCactusAttack dT posP = triggerEvery dT 1 0.6 $ do
    cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 0) $
        (modify etyE $ \(Enemy _ _, hp) -> dealDamage hp cactusDmg)

doBigMushroomAttack :: (HasMany w [Enemy, Position, Plant, Time, Hp, AnimatedSprite, EntityCounter, Particle]) => Float -> V2 Float -> System w ()
doBigMushroomAttack dT posP = triggerEvery dT 2 0.6 $ do
    cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 2) $ do
        newEntity (Position (posP), aoeEffect, Particle 2)
        modify etyE (\(Enemy _ _, hp) -> dealDamage hp bigMushroomDmg)

doEnchanting :: (HasMany w [Plant, Position, Time, Hp, Sprite, Particle, EntityCounter]) => Float -> V2 Float -> System w ()
doEnchanting dT posEch = triggerEvery dT 8 0.6 $ do
    cmapM_ $ \(_::Plant, Position posP, etyP) -> when (L.norm (posEch - posP) < tileRange 1) $ 
        (do
            modify etyP $ \(_::Plant, hp) -> shieldHp hp enchanterShield
            xoff <- liftIO $ randomRIO (-16,16)
            yoff <- liftIO $ randomRIO (-16,16) 
            void $ newEntity (Sprite shieldEffect, Position (posP+V2 xoff yoff), Particle 2))

doSeedSeeking :: (HasMany w [Seed, Sprite, Plant, Position, Time, EntityCounter]) => Float -> V2 Float -> System w ()
doSeedSeeking dT pos = triggerEvery dT 0.5 0.6 $ do
    seed <- liftIO $ randomRIO (0,3)
    xoff <- liftIO $ randomRIO (-32,32)
    yoff <- liftIO $ randomRIO (-32,-1)
    newEntity ([GreenSeed,RedSeed,BlueSeed,Spore]!! seed, Position (V2 xoff yoff + pos), Sprite $ [greenSeed,redSeed,blueSeed,spore] !! seed )



doLazerAttack :: (HasMany w [Enemy, Position, Time, Hp, Particle, Sprite, EntityCounter]) => Float -> V2 Float -> System w ()
doLazerAttack dT posP = triggerEvery dT 2 0.6 $ do
    (cdist, closest) <- cfold (\min@(minDist,_) (Enemy _ _, Position posE, etyE) ->
            let nDist = L.norm (posP - posE)
            in if nDist < minDist then (nDist,etyE) else min) (10000,0)
    when (cdist < tileRange 2) $ do
        Position posE <- get closest
        let V2 lx ly = posE - posP
            (ox,oy) = if lx< 0 then (0,0) else (0,0) --TODO:origin of lazer
            lazerLine = (color orange $ Line [(ox,oy),(lx,ly)]) <> (color red $ Line [(ox,oy+2),(lx,ly)]) <> (color red $ Line [(ox,oy-2),(lx,ly)])
        newEntity (Particle 0.25, Position posP, Sprite lazerLine)
        modify closest $ \(Enemy _ _, hp) -> dealDamage hp lazerDmg
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


destroyDeadStructures :: (HasMany w [Sprite, Plant, Position, Paths, PathFinder, Structure, Hp, Camera]) => System w ()
destroyDeadStructures = do
    pathingChanged <- cfoldM (\b (Structure _, Hp hp _ _ , ety, Position pos) -> do
        when (hp <= 0) $ do
            playIOSoundEffectAt pos explosion
            destroy ety (Proxy @AllPlantComps )
        return (b || hp <= 0)) False
    when pathingChanged $ do
        updateGoals
        clearPaths
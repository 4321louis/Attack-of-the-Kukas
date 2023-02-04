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
import Apecs.Extension
import Apecs.Gloss 
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

type AllPlantComps = (Position, Structure, Sprite, Hp, Plant)

data Plant = RockPlant | Cactus | BigMushroom | Enchanter | SeedSeeker | CorpseFlower | VampireFlower | BirdOfParadise | Mycelium deriving (Show)
instance Component Plant where type Storage Plant = Map Plant

cactusDmg, enchanterShield :: Float
cactusDmg = 20
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
getPlant [Spore, Spore] = Mycelium
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

doPlants :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, Seed])=> Float -> System w ()
doPlants dT = cmapM_ $ \(plant::Plant, Position pos) ->
    case plant of
        Cactus -> doCactusAttack dT pos
        Enchanter -> doEnchanting dT pos
        SeedSeeker -> doSeedSeeking dT pos
        --do GB - Healer
        --do GR - Attackspeed
        --do GS - Vampiric 
        --do BB - ROCK PLANT
        --do BR - Cactus
        --do BS - Aoe mushroom, corpses fume as well
        --do RR - Lazer/swatter
        --do RS - Damage over time
        --do SS - Necromancy
        _ -> do return ()

doCactusAttack :: (HasMany w [Enemy, Position, Plant, Time, Hp]) => Float -> V2 Float -> System w ()
doCactusAttack dT posP = do
    cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 0) $
        triggerEvery dT 1 0.6 (modify etyE $ \(Enemy _ _, hp) -> dealDamage hp cactusDmg)

doEnchanting :: (HasMany w [Plant, Position, Time, Hp]) => Float -> V2 Float -> System w ()
doEnchanting dT posEch = do
    cmapM_ $ \(_::Plant, Position posP, etyP) -> when (L.norm (posEch - posP) < tileRange 1) $
        triggerEvery dT 1 0.6 (modify etyP $ \(_::Plant, hp) -> shieldHp hp enchanterShield)

doSeedSeeking :: (HasMany w [Seed, Sprite, Plant, Position, Time, EntityCounter]) => Float -> V2 Float -> System w ()
doSeedSeeking dT pos = do
    seed <- liftIO $ randomRIO (0,3)
    xoff <- liftIO $ randomRIO (-32,32)
    yoff <- liftIO $ randomRIO (-32,-1)
    triggerEvery dT 30 0.6 $ newEntity ([GreenSeed,RedSeed,BlueSeed,Spore]!! seed, Position (V2 xoff yoff + pos), Sprite $ [greenSeed,redSeed,blueSeed,spore] !! seed )


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
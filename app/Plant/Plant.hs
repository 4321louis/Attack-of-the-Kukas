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
import Apecs.Gloss 
import Control.Monad
import Misc
import System.Random
import qualified Linear as L

import Audio
import Enemy.Enemy
import Enemy.Hive
import Enemy.Pathfinding
import Worlds
import Plant.Seed
import Linear (V2(..))
import Structure.Structure
import Drawing.Sprites (targetSprite1, attackSpeedEffect)

type AllPlantComps = (Position, Structure, Sprite, Hp, Plant)

data Plant = RockPlant | Cactus | BigMushroom | Enchanter | SeedSeeker | CorpseFlower | VampireFlower | BirdOfParadise | Mycelium | Necromancer deriving (Show, Eq)
instance Component Plant where type Storage Plant = Map Plant

-- Dmg, Fuse, speed
data UndeadBomber = UndeadBomber Float Float Float
instance Component UndeadBomber where type Storage UndeadBomber = Map UndeadBomber
--Timer
data AttackSpeed = AttackSpeed Float deriving (Show)
instance Component AttackSpeed where type Storage AttackSpeed = Map AttackSpeed

bigMushroomDmg, cactusDmg, enchanterShield :: Float
bigMushroomDmg = 10
cactusDmg = 20
lazerDmg = 100
enchanterShield = 5
attackSpeedModifier = 3

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
getPlantSprite Necromancer = mycelium
getPlantSprite _ = seedSeeker

newPlant :: (HasMany w [Plant, Position, Hp, Sprite, Structure, EntityCounter, AttackSpeed]) => Plant -> V2 Float -> System w Entity
newPlant Cactus pos = newEntity (Cactus, Position pos, Hp 20 20 0, Sprite cactus)
newPlant Enchanter pos = newEntity (Enchanter, Position pos, Hp 4 4 0, Sprite enchanter, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant SeedSeeker pos = newEntity (SeedSeeker, Position pos, Hp 20 20 0, Sprite seedSeeker, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant RockPlant pos = newEntity (RockPlant, Position pos, Hp 80 80 0, Sprite rockPlant, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant CorpseFlower pos = newEntity (CorpseFlower, Position pos, Hp 20 20 0, Sprite attackSpeedFlower, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant VampireFlower pos = newEntity (VampireFlower, Position pos, Hp 20 20 0, Sprite vampireFlower, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant BigMushroom pos = newEntity (BigMushroom, Position pos, Hp 20 20 0, Sprite aoeMushroom, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant BirdOfParadise pos = newEntity (BirdOfParadise, Position pos, Hp 20 20 0, Sprite birdOfParadise, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]), AttackSpeed 0)
newPlant Mycelium pos = newEntity (Mycelium, Position pos, Hp 20 20 0, Sprite mycelium, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))
newPlant Necromancer pos = newEntity (Necromancer, Position pos, Hp 20 20 0, Sprite mycelium, Structure ((pos+) <$> [V2 64 0, V2 (-64) 0, V2 0 64,V2 0 (-64)]))

doPlants :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, AttackSpeed, Seed, Particle, UndeadBomber, PathFinder, Hive, Velocity, AnimatedSprite])=> Float -> System w ()
doPlants dT = do 
    doAttacks dT
    doOnDeaths

doAttacks :: (HasMany w [Enemy, Position, Plant, Time, Hp, EntityCounter, Sprite, AttackSpeed, Seed, Particle, AnimatedSprite, UndeadBomber, PathFinder, Hive, Velocity])=> Float -> System w ()
doAttacks dT = do
    cmapM_ $ \(plant::Plant, Position pos, ety) -> do
        case plant of
            Cactus -> doCactusAttack dT pos ety
            Enchanter -> doEnchanting dT pos
            SeedSeeker -> doSeedSeeking dT pos
            BigMushroom -> doBigMushroomAttack dT pos ety
            BirdOfParadise -> doLazerAttack dT pos ety
            RockPlant -> doRockPlant dT ety
            CorpseFlower -> doAttackSpeedBuff dT pos
            --do GS - Vampiric 
            --do RS - Damage over time ety
            _ -> return ()
    stepAttackSpeedTimer dT
    doUndeadBombers dT

doUndeadBombers dT = cmapM $ 
    \(p@(PathFinder _ pathNodes), Position pos, Velocity _, UndeadBomber dmg fuse speed) ->
        if fuse <= 0 
        then do  
            cmap $ \(Enemy _ _, Position posE, hp) -> if (L.norm (posE - pos) < tileRange 1) then dealDamage hp dmg else hp
            newEntity (Position (pos), aoeEffect, Particle 2)
            return $ Right (Not @(Sprite,PathFinder,Position,Velocity,UndeadBomber))
        else do
            newFuse <- (`cfold` fuse) $ \f (Enemy _ _,Position posE) -> if (L.norm (posE - pos) < tileRange 1) then f - dT else f
            if null pathNodes
            then return $ Left (PathFinder Nothing [],Velocity (V2 0 0),UndeadBomber dmg newFuse speed)
            else return $ Left (p,Velocity ((L.^* speed) . L.normalize $ head pathNodes - pos),UndeadBomber dmg newFuse  speed)

doOnDeaths :: (HasMany w [Enemy, Position, Velocity, UndeadBomber, PathFinder, Hive, Plant, Time, Hp, EntityCounter, Sprite, Seed, Particle]) => System w ()
doOnDeaths = do
    (deadPositions, deadEnemies) <- cfold (\(poss, etys) (Enemy _ _, Hp hp _ _,ety::Entity, Position pos)-> if hp <=0 then (pos:poss,ety:etys) else (poss,etys)) ([],[])
    cmapM_ $ \(plant::Plant, Position pos, ety::Entity) ->
        case plant of
            -- BigMushroom -> domush deadEnemies
            --do GS - Vampiric 
            Necromancer -> necromancyOnDeath pos deadEnemies
            _ -> do return ()

doRockPlant :: (HasMany w [Time, Hp]) => Float -> Entity -> System w ()
doRockPlant dT ety = triggerEvery dT 1 0.6 $ modify ety (`healHp` 1)

doAttackSpeedBuff :: (HasMany w [Plant, Position, Time, Hp, Sprite, Particle, EntityCounter, AttackSpeed]) => Float -> V2 Float -> System w ()
doAttackSpeedBuff dT pos = triggerEvery dT 2 0.6 $ do
    cmapM $  \(p::Plant, Position posP) -> if L.norm (pos - posP) < tileRange 1 && elem p [Cactus, BirdOfParadise, BigMushroom, Mycelium, VampireFlower] then do 
        xoff <- liftIO $ randomRIO (-16,16)
        yoff <- liftIO $ randomRIO (-16,16) 
        newEntity (Sprite attackSpeedEffect, Position (posP+V2 xoff yoff), Particle 1) 
        return $ Right $ AttackSpeed 4
        else return $ Left ()

stepAttackSpeedTimer :: (Has w IO AttackSpeed) => Float -> System w ()
stepAttackSpeedTimer dT = cmap $ \(AttackSpeed t) ->
    if t < 0
        then Right $ Not @AttackSpeed
        else Left  $ AttackSpeed (t-dT)

doCactusAttack :: (HasMany w [Enemy, Position, Plant, Time, Hp, AttackSpeed]) => Float -> V2 Float -> Entity -> System w ()
doCactusAttack dT posP ety = do
    hasAttackBuff <- exists ety (Proxy @AttackSpeed)
    let rate = 1/if hasAttackBuff then attackSpeedModifier else 1 
    triggerEvery dT rate 0.6 $ do
        cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 0) $
            modify etyE (\(Enemy _ _, hp) -> dealDamage hp cactusDmg)

doBigMushroomAttack :: (HasMany w [Enemy, Position, Plant, Time, Hp, AnimatedSprite, EntityCounter, Particle,AttackSpeed]) => Float -> V2 Float -> Entity -> System w ()
doBigMushroomAttack dT posP ety = do
    hasAttackBuff <- exists ety (Proxy @AttackSpeed)
    let rate = 2/if hasAttackBuff then attackSpeedModifier else 1 
    triggerEvery dT rate 0.6 $ do
        cmapM_ $ \(Enemy _ _, Position posE, etyE) -> when (L.norm (posE - posP) < tileRange 2) $ do
            newEntity (Position posP, aoeEffect, Particle 2)
            modify etyE (\(Enemy _ _, hp) -> dealDamage hp bigMushroomDmg)

{- doBigMushroomOnDeath :: (HasMany w [Enemy, Position, Plant, Time, Hp, AnimatedSprite, EntityCounter, Particle]) => [Entity] -> V2 Float -> System w ()
doBigMushroomOnDeath deadEnemies =
    cmapM_ $ \(Plant, ) -}

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



doLazerAttack :: (HasMany w [Enemy, Position, Time, Hp, Particle, Sprite, EntityCounter, AttackSpeed]) => Float -> V2 Float -> Entity -> System w ()
doLazerAttack dT posP ety = do
    hasAttackBuff <- exists ety (Proxy @AttackSpeed)
    let rate = 2/if hasAttackBuff then attackSpeedModifier else 1 
    triggerEvery dT rate 0.6 $ do
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

necromancyOnDeath :: (HasMany w [Enemy, Position, Velocity, UndeadBomber, PathFinder, Hive, Time, Hp, Particle, Sprite, EntityCounter]) => V2 Float -> [Entity] -> System w ()
necromancyOnDeath posP deads = (\func -> foldM func () deads) $ \b ety -> do
    (Position pos, Hp _ maxHp _, Enemy _ speed) <- get ety
    when (L.norm (posP - pos) < tileRange 4) $ do
        hives <- (`cfold` []) $ \hs (h::Hive, Position pos) -> pos:hs
        void $ newEntity (Position pos, Velocity (V2 0 0), UndeadBomber (maxHp/3) 4 speed, Sprite targetSprite1, PathFinder (Just hives) [])


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
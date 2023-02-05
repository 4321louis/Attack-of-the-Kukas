--Hives and waves that spawn locusts

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

module Enemy.Hive where

import Apecs
import Control.Monad
import Linear (V2(..))

import Debug.Trace
import Drawing.Sprites
import Misc
import Worlds
import Apecs.Extension
import Grid.Implementation
import Enemy.Enemy
import Enemy.Pathfinding

initializeHives :: (HasMany w [Hive, Time, Hp, Enemy, Position, Sprite, Velocity, PathFinder, EntityCounter]) => Int -> [(Int,Int)] -> System w ()
initializeHives size poses = do
    rposes <- liftIO $ shuffleList poses
    foldM_ (\_ (hive,pos) -> newEntity (Position (tileCentre 2 $ toRealCoord size pos),hive)) 0 $
        zip [Hive1, Hive2, Hive3, Hive4, Hive5] rposes

data Hive = Hive1 | Hive2 | Hive3 | Hive4 | Hive5
instance Component Hive where type Storage Hive = Map Hive

spawnEnemies :: HasMany w [Hive, Position, Time, Hp, Enemy, Sprite, AnimatedSprite, Velocity, PathFinder, EntityCounter] => Float -> System w ()
spawnEnemies dT =
    cmapM $ \(h, Position pos) -> spawnsHive h dT pos

spawnsHive :: (HasMany w [Time, Hp, Enemy, Position, Sprite, AnimatedSprite, Velocity, PathFinder, EntityCounter]) => Hive -> Float -> V2 Float -> System w ()
spawnsHive Hive1 dT pos = do
    spawnWave 20 dT 3 1.3 $ getEnemyFromType Normal 1 pos
    spawnWave 40 dT 3 1.3 $ getEnemyFromType Normal 1 pos

    doMinute 4 dT 3 3 pos Normal
    doMinute 7 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 10 dT 3 3 pos Fast
    doMinute 13 dT 8 3 pos Normal
    doMinute 15 dT 5 3 pos Fast

spawnsHive Hive2 dT pos = do
    doMinute 1 dT 3 3 pos Normal
    doMinute 4 dT 3 3 pos Normal
    doMinute 7 dT 5 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 11 dT 8 3 pos Normal
    doMinute 13 dT 3 3 pos Tank
    doMinute 15 dT 3 3 pos Tank

spawnsHive Hive3 dT pos = do
    doMinute 2 dT 5 3 pos Normal
    doMinute 5 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 9 dT 5 3 pos Normal
    doMinute 11 dT 5 3 pos Fast
    doMinute 14 dT 8 3 pos Normal

spawnsHive Hive4 dT pos = do
    doMinute 3 dT 5 3 pos Normal
    doMinute 6 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 9 dT 3 3 pos Fast
    doMinute 12 dT 5 3 pos Normal
    doMinute 14 dT 5 3 pos Tank

spawnsHive Hive5 dT pos = do
    doMinute 4 dT 3 3 pos Normal
    doMinute 6 dT 3 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 10 dT 8 3 pos Normal
    doMinute 12 dT 3 3 pos Tank
    doMinute 15 dT 8 3 pos Normal

doMinute :: HasMany w [Time, Hp, Enemy, Position, Sprite, AnimatedSprite, Velocity, PathFinder, EntityCounter] => Int -> Float -> Float -> Float -> V2 Float -> EnemyType -> System w ()
doMinute minute dT enemies waves pos etype = do
    let scaling = 1+0.2*fromIntegral (div minute 150)
        sec = 60 * fromIntegral minute
    foldM_ (\_ e -> e) () [spawnWave time dT enemies 1.3 (getEnemyFromType etype scaling pos) | time <- [sec, sec + 60/waves .. sec + 59]]



spawnWave :: Has w IO Time => Float -> Float -> Float -> Float -> System w a -> System w ()
spawnWave time dT amount rate system = do
    foldM_ (\_ etime ->triggerAt dT etime system) () [time,time+rate .. time+(amount-1)*rate]

getEnemyFromType :: HasMany w  [Hp, Enemy, Position, Sprite, AnimatedSprite, Velocity, PathFinder, EntityCounter] => EnemyType -> Float -> V2 Float -> System w Entity
getEnemyFromType Normal scaling pos = newEntity (Hp (100*scaling) (100*scaling) 0, Enemy 1 35 Normal, Position pos, droneKukasWalkRight, Velocity (V2 0 0), PathFinder Nothing [])
getEnemyFromType Fast scaling pos = newEntity (Hp (50*scaling) (50*scaling) 0, Enemy 1 45 Fast, Position pos, armourKukasWalkRight, Velocity (V2 0 0), PathFinder Nothing [])
getEnemyFromType Tank scaling pos = newEntity (Hp (200*scaling) (200*scaling) 0, Enemy 1 25 Tank, Position pos, fastKukasWalkRight, Velocity (V2 0 0), PathFinder Nothing [])
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
import Apecs.Gloss
import Audio
import Codec.Picture
import Control.Monad
import Debug.Trace (trace)
import Debug.Time
import qualified Data.Map as M
import GHC.Exts (sortWith)
import Graphics.Gloss.Export.Image
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Interface.Environment
import Linear (V2(..))
import qualified Linear as L
import Sound.ProteaAudio


import Drawing.Sprites
import Drawing.Camera
import Misc
import Player
import Worlds
import Apecs.Extension
import Structure.Structure
import Grid.Implementation
import Grid.Tile
import Enemy.Enemy
import Enemy.Pathfinding
import Plant.Plant
import Plant.Seed
import Drawing.Sprites (spriteDir)

initializeHives :: (HasMany w [Hive, Time, Hp, Enemy, Position, Sprite, Velocity, PathFinder, EntityCounter]) => Int -> [(Int,Int)] -> System w ()
initializeHives size poses = do
    rposes <- liftIO $ shuffleList poses
    foldM_ (\b (hive,pos@(x,y)) -> newEntity (Position (toRealCoord size pos),hive)) 0 $ 
        zip [Hive1, Hive2, Hive3, Hive4, Hive5] $ rposes

data Hive = Hive1 | Hive2 | Hive3 | Hive4 | Hive5
instance Component Hive where type Storage Hive = Map Hive

spawnEnemies dT =
    cmapM $ \(h, Position pos) -> spawnsHive h dT pos

spawnsHive :: (HasMany w [Time, Hp, Enemy, Position, Sprite, Velocity, PathFinder, EntityCounter]) => Hive -> Float -> V2 Float -> System w ()
spawnsHive Hive1 dT pos = do
    triggerEvery dT 8 3 $ newEntity (Hp 100 100 0, Enemy 1 20, Position pos, Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

spawnsHive Hive2 dT pos = do
    triggerEvery dT 8 1 $ newEntity (Hp 100 100 0, Enemy 1 20, Position pos, Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

spawnsHive Hive3 dT pos = do
    triggerEvery dT 8 5 $ newEntity (Hp 100 100 0, Enemy 1 20, Position pos, Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

spawnsHive Hive4 dT pos = do
    triggerEvery dT 8 7 $ newEntity (Hp 100 100 0, Enemy 1 20, Position pos, Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

spawnsHive Hive5 dT pos = return ()


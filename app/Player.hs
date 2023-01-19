--Player and IO controlling

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

module Player where

import Apecs
import Apecs.Gloss 
import Linear (V2(..))
import qualified Linear as L
import qualified Data.Set as S
import System.Exit

import Worlds
import Drawing.Sprites
import Apecs.Extension

data Player = Player deriving (Show)
instance Component Player where type Storage Player = Unique Player

data Inputs = Inputs (S.Set Key) (V2 Float) deriving (Show)
instance Semigroup Inputs where (Inputs a v1) <> (Inputs b v2) = Inputs (S.union a b) (v1+v2)
instance Monoid Inputs where mempty = Inputs S.empty (V2 0.00000001 0)
instance Component Inputs where type Storage Inputs = Global Inputs

data Bullet = Bullet deriving (Show)
instance Component Bullet where type Storage Bullet = Map Bullet

playerSpeed, bulletSpeed :: Float 
playerSpeed = 170
bulletSpeed = 500

rotatePlayer :: (HasMany w [Player,Sprite,Inputs]) => System w ()
rotatePlayer = do
    Inputs _ (V2 xPos yPos) <- get global
    cmap $ \(Player, Sprite _) -> Sprite $ rotate (-180 / pi * (atan (yPos / xPos) + if xPos >= 0 then 0 else pi)) playerSprite

preHandleEvent :: (HasMany w [Player, Position, Velocity, Inputs, Bullet, Particle, EntityCounter, Sprite]) => Event -> System w ()
preHandleEvent e@(EventKey k Down _ _) = do
    modify global $ \(Inputs s m) -> Inputs (S.insert k s) m
    handleEvent e
preHandleEvent e@(EventKey k Up _ _) = do
    modify global $ \(Inputs s m) -> Inputs (S.delete k s) m
    handleEvent e
preHandleEvent e@(EventMotion (x, y)) = do
    modify global $ \(Inputs s _) -> Inputs s (V2 x y)
    handleEvent e
preHandleEvent e = handleEvent e

handleEvent :: (HasMany w [Player, Position, Velocity, Inputs, Bullet, Particle, Sprite, EntityCounter]) => Event -> System w ()
handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyRight) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) =
    cmapM_ $ \(Player, pos@(Position _)) -> do
        let mouseAngle = atan (yPos / xPos) + if xPos >= 0 then 0 else pi
        _bullet <- newEntity (Bullet, pos, Velocity (bulletSpeed L.*^ V2 (cos mouseAngle) (sin mouseAngle)), Sprite bulletSprite)
        spawnParticles 7 pos (-80, 80) (10, 100)
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = return ()

playerVelocityfromInputs :: S.Set Key -> V2 Float
playerVelocityfromInputs inputs =
    let hdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyRight) inputs) - fromEnum (S.member (SpecialKey KeyLeft) inputs)
        vdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyUp) inputs) - fromEnum (S.member (SpecialKey KeyDown) inputs)
        direction = V2 hdirection vdirection L.^* if hdirection == 0 || vdirection == 0 then 1 else 0.707106781
    in direction L.^* playerSpeed

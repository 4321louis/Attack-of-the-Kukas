--Player and IO controlling

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Grid.Tile
import Grid.Implementation
import Worlds
import Drawing.Sprites
import Apecs.Extension
import Data.Maybe
import Misc

import Debug.Trace  (trace)
import Control.Monad
import Structure.Structure
import Plant.Plant
import Plant.Seed
import Enemy.Pathfinding

data Player = Player deriving (Show)
instance Component Player where type Storage Player = Unique Player

-- heldkeys, mouse position, mouse delta
data Inputs = Inputs (S.Set Key) (V2 Float) (V2 Float) deriving (Show)
instance Semigroup Inputs where (Inputs a p1 d1) <> (Inputs b p2 d2) = Inputs (S.union a b) (p1+p2) (d1+d2)
instance Monoid Inputs where mempty = Inputs S.empty (V2 0.00000001 0) (V2 0 0)
instance Component Inputs where type Storage Inputs = Global Inputs

playerSpeed :: Float
playerSpeed = 170

handleInputs :: (HasMany w [Seed,Enchanter, Cactus, Plant, Hp, Player, Position, Velocity, Inputs, Camera, EntityCounter, MapGrid, Sprite, Structure, Paths, PathFinder]) => Event -> System w ()
handleInputs e = do
    modify global $ \(Inputs s m _) -> Inputs s m (V2 0 0)
    updateGlobalInputs e
    handleEvent e
    doMousePanning

updateGlobalInputs :: (Has w IO Inputs) => Event -> System w ()
updateGlobalInputs (EventKey k Down _ _) = do
    modify global $ \(Inputs s m d) -> Inputs (S.insert k s) m d
updateGlobalInputs (EventKey k Up _ _) = do
    modify global $ \(Inputs s m d) -> Inputs (S.delete k s) m d
updateGlobalInputs (EventMotion (x, y)) = do
    modify global $ \(Inputs s prev _) -> Inputs s (V2 x y) (V2 x y - prev)
updateGlobalInputs _ = return ()

handleEvent :: (HasMany w [Seed,Enchanter, Cactus, Plant, Hp, Player, Velocity, Inputs, EntityCounter, MapGrid, Position, Sprite, Camera, Structure, Paths, PathFinder]) => Event -> System w ()
handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyRight) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (MouseButton LeftButton) Down _ _) = cmapM_ $ \(Player, Inputs _ cursorPos _, MapGrid grid size, Camera pos scale ) -> plantPlants pos cursorPos grid size scale
handleEvent (EventKey (MouseButton RightButton) Down _ _) = cmapM $ \all@(Seed, Position sPos, Sprite _, Inputs _ mPos _, camera) -> if L.norm (cursorPosToReal camera mPos - sPos) < 24
    then do
        return $ Right (Not @(Seed, Position, Sprite))
    else return $ Left ()
handleEvent _ = return ()

playerVelocityfromInputs :: S.Set Key -> V2 Float
playerVelocityfromInputs inputs =
    let hdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyRight) inputs) - fromEnum (S.member (SpecialKey KeyLeft) inputs)
        vdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyUp) inputs) - fromEnum (S.member (SpecialKey KeyDown) inputs)
        direction = V2 hdirection vdirection L.^* if hdirection == 0 || vdirection == 0 then 1 else 0.707106781
    in direction L.^* playerSpeed

doMousePanning :: (HasMany w [Player, Position, Camera, Inputs]) => System w ()
doMousePanning = cmap $ \(Player, Position p, Inputs keys _ d,Camera _ cscale) -> if S.member (MouseButton MiddleButton) keys then Position (p - (d L.^/ cscale)) else Position p


plantPlants ::  (HasMany w [Enchanter, Cactus, Plant, Position, Hp, Sprite, Structure, EntityCounter, Camera, Paths, PathFinder]) => V2 Float -> V2 Float -> Grid -> Int -> Float -> System w ()
plantPlants playerPos cursorPos grid size scale =
    when (placeable tile) $ do
        -- _plant <- newEntity (Cactus, Position (V2 cenX cenY), Sprite cactus)
        _plant <- newEntity (Enchanter, Plant, Position (V2 cenX cenY), Hp 1 1 0, Sprite enchanter, Structure [(cenX+64,cenY),(cenX-64,cenY),(cenX,cenY+64),(cenX,cenY-64)])
        updateGoals
        clearPaths
    where   V2 a b = cursorPos
            (V2 x y) = playerPos + V2 (a/scale) (b/scale)
            (cenX, cenY) = tileCentre size (x, y)
            tile = fromMaybe erTile3 $ tileOfCoord grid size (x, y)

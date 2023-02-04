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

import Audio
import Debug.Trace  (trace)
import Control.Monad
import Structure.Structure
import Plant.Plant
import Plant.Seed
import Enemy.Pathfinding
import System.Random

data Player = Player deriving (Show)
instance Component Player where type Storage Player = Unique Player

-- heldkeys, mouse position, mouse delta
data Inputs = Inputs (S.Set Key) (V2 Float) (V2 Float) deriving (Show)
instance Semigroup Inputs where (Inputs a p1 d1) <> (Inputs b p2 d2) = Inputs (S.union a b) (p1+p2) (d1+d2)
instance Monoid Inputs where mempty = Inputs S.empty (V2 0.00000001 0) (V2 0 0)
instance Component Inputs where type Storage Inputs = Global Inputs

playerSpeed :: Float
playerSpeed = 170

handleInputs :: (HasMany w [Inventory, Seed, Plant, Hp, Player, Position, Velocity, Inputs, Camera, EntityCounter, MapGrid, Sprite, Structure, Paths, PathFinder]) => Event -> System w ()
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

handleEvent :: (HasMany w [Inventory, Seed, Plant, Hp, Player, Velocity, Inputs, EntityCounter, MapGrid, Position, Sprite, Camera, Structure, Paths, PathFinder]) => Event -> System w ()
handleEvent (EventKey (SpecialKey KeyLeft) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyRight) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) = cmap $ \(Player, Velocity _, Inputs s _ _) -> Velocity (playerVelocityfromInputs s)

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess

handleEvent (EventKey (Char 'q') Down _ _) = modify global $ \(Inventory inv (seed:seeds)) -> Inventory inv [GreenSeed, seed]
handleEvent (EventKey (Char 'w') Down _ _) = modify global $ \(Inventory inv (seed:seeds)) -> Inventory inv [RedSeed, seed]
handleEvent (EventKey (Char 'e') Down _ _) = modify global $ \(Inventory inv (seed:seeds)) -> Inventory inv [BlueSeed, seed]
handleEvent (EventKey (Char 'r') Down _ _) = modify global $ \(Inventory inv (seed:seeds)) -> Inventory inv [Spore, seed]


handleEvent (EventKey (MouseButton LeftButton) Down modifiers _) = cmapM_ $ \(Player, Inputs _ cursorPos _, MapGrid grid size, cam ) -> 
    if Up == alt modifiers
    then plantPlants cam cursorPos grid size
    else removePlant cam cursorPos
handleEvent (EventKey (MouseButton RightButton) Down _ _) = cmapM $ \all@(s::Seed, Position sPos, Sprite _, Inputs _ mPos _, camera) -> 
    if L.norm (cursorPosToReal camera mPos - sPos) < 24
    then do
        playIOSoundEffect pickUpSeed
        modify global $ \(Inventory inv seeds) -> Inventory (inventoryAdd inv s) seeds
        return $ Right (Not @(Seed, Position, Sprite))
    else return $ Left ()
handleEvent _ = return ()

inventoryAdd :: [Int] -> Seed -> [Int]
inventoryAdd [g, r, b, s] GreenSeed = [g+1, r, b, s]
inventoryAdd [g, r, b, s] RedSeed = [g, r+1, b, s]
inventoryAdd [g, r, b, s] BlueSeed = [g, r, b+1, s]
inventoryAdd [g, r, b, s] Spore = [g, r, b, s+1]
inventoryAdd other _ = other -- i promise i will fix this poorly designed system in beta

playerVelocityfromInputs :: S.Set Key -> V2 Float
playerVelocityfromInputs inputs =
    let hdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyRight) inputs) - fromEnum (S.member (SpecialKey KeyLeft) inputs)
        vdirection = fromIntegral $ fromEnum (S.member (SpecialKey KeyUp) inputs) - fromEnum (S.member (SpecialKey KeyDown) inputs)
        direction = V2 hdirection vdirection L.^* if hdirection == 0 || vdirection == 0 then 1 else 0.707106781
    in direction L.^* playerSpeed

doMousePanning :: (HasMany w [Player, Position, Camera, Inputs]) => System w ()
doMousePanning = cmap $ \(Player, Position p, Inputs keys _ d,Camera _ cscale) -> if S.member (MouseButton MiddleButton) keys then Position (p - (d L.^/ cscale)) else Position p


-- Plants a plant (entity) on the cursor position
plantPlants ::  (HasMany w [Inventory, Plant, Position, Hp, Sprite, Structure, EntityCounter, Camera, Paths, PathFinder]) => Camera -> V2 Float -> Grid -> Int -> System w ()
plantPlants cam cursorPos grid size = do
    Inventory inv craft <- get 0
    
    hasPlant <- hasEntity plantPos
    -- check inventory
    let inv' = updateInv inv craft
        validInv = all (>=0) inv'
    when (placeable tile && not hasPlant && not validInv) $ do
        -- todo: play some sort of sfx
        void $ playIOSoundEffect plantPlant

    -- attempt to plant
    when (placeable tile && not hasPlant && validInv) $ do
        xoff <- liftIO $ randomRIO (-8,8)
        yoff <- liftIO $ randomRIO (-8,8)
        let plant = getPlant craft
        _plant <- newPlant plant (plantPos + V2 xoff yoff)
        modify global $ \(Inventory _ seeds) -> Inventory inv' seeds
        playIOSoundEffect plantPlant
        updateGoals
        clearPaths
    where   realCursorPos = cursorPosToReal cam cursorPos
            plantPos = tileCentre 2 realCursorPos
            tile = fromMaybe erTile3 $ tileOfCoord grid size realCursorPos

updateInv :: [Int] -> [Seed] -> [Int]
updateInv [g, r, b, s] [] = [g, r, b, s]
updateInv [g, r, b, s] (c:cs) = case c of 
    GreenSeed -> updateInv [g-1, r, b, s] cs
    RedSeed -> updateInv [g, r-1, b, s] cs
    BlueSeed -> updateInv [g, r, b-1, s] cs
    Spore -> updateInv [g, r, b, s-1] cs
updateInv lst _ = lst

removePlant :: HasMany w [Position, Plant, Structure, Sprite, Hp, Paths, PathFinder] => Camera -> V2 Float -> SystemT w IO ()
removePlant cam cursorPos = cmapM_ $ \(Position pos, _::Plant, ety) -> 
    when (L.norm (pos - tileCentre 2 (cursorPosToReal cam cursorPos )) < 10) $ do
        destroy ety (Proxy @AllPlantComps )        
        updateGoals
        clearPaths

-- Checks if entity exists on real coord
-- Could expand to return the entities on the tile coord (in the future?)
hasEntity :: (HasMany w [EntityCounter, Position, Plant]) => V2 Float -> SystemT w IO Bool
hasEntity vectorPos = cfold (\bool (Position pos, _::Plant) -> bool || 11.4 >= L.norm (pos-vectorPos)) False
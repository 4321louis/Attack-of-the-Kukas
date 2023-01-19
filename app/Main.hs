--Putting it all together and some more specific features

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


import Apecs
import Apecs.Gloss
import Control.Monad
import Linear (V2(..))
import qualified Linear as L

import Drawing.Sprites
import Drawing.Camera
import Misc
import Player
import Worlds
import Apecs.Extension
import Tile
import qualified Data.Map as M

data Target = Target deriving (Show)
instance Component Target where type Storage Target = Map Target


makeWorld "World" [''Position, ''Velocity, ''MovementPattern, ''Sprite, ''AnimatedSprite, ''Player, ''Target, ''Bullet, ''Particle, ''Score, ''Time, ''Inputs, ''Camera]


type SystemW a = System World a

xmin, xmax :: Float
xmin = -110
xmax = 110

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 (-40)
scorePos = V2 xmin (-170)

initialize :: Grid -> [(Int, Int)] -> SystemW ()
initialize grid coords = do
    initialiseGrid grid coords
    _playerEty <- newEntity (Player, Position playerPos, Velocity 0, Sprite playerSprite)
    return ()

initialiseGrid :: (HasMany w [Tile, Position, Velocity, EntityCounter, Sprite]) => Grid -> [(Int,Int)] -> System w ()
initialiseGrid grid coords  = do
    mapM_ void [newEntity (Position (V2 (fromIntegral x) (fromIntegral y)), Sprite $ getPic (M.findWithDefault erTile (x,y) grid))| (x,y) <-coords]

clampPlayer :: SystemW ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) ->
    Position (V2 (min xmax . max xmin $ x) y)

clearTargets :: SystemW ()
clearTargets = cmap $ \entity@(Target, Position (V2 x _), MovementPattern _) ->
    if x < xmin || x > xmax
        then Nothing
        else Just entity

clearBullets :: SystemW ()
clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
    if y > 170
        then Right (Not @(Bullet, Kinetic), Score (s - missPenalty))
        else Left ()

handleCollisions :: SystemW ()
handleCollisions =
    cmapM_ $ \(Target, Position posT, etyT) ->
        cmapM_ $ \(Bullet, Position posB, etyB) ->
            when (L.norm (posT - posB) < 10) $ do
                destroy etyT (Proxy @(Target, Kinetic))
                destroy etyB (Proxy @(Bullet, Kinetic))
                spawnParticles 15 (Position posB) (-500, 500) (200, -50)
                modify global $ \(Score x) -> Score (x + hitBonus)
                modify global $ \(Camera pos cScale) -> Camera pos (0.85*cScale)

step :: Float -> SystemW ()
step dT = do
    incrTime dT
    stepPosition dT
    -- clampPlayer
    animatedSprites dT
    rotatePlayer
    -- clearTargets
    clearBullets
    stepParticles dT
    handleCollisions
    camOnPlayer
    rescaleCam dT
    Time toffset <- get global
    triggerEvery dT 0.6 0 $ newEntity (Target, Position (V2 xmin 80), MovementPattern (orbitalPattern (V2 0 0) 130 5 toffset) , animTargetSprite1)
    triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), MovementPattern (starPattern (V2 0 0) 130 5 toffset) , animTargetSprite2)

draw :: SystemW Picture
draw = do
    sprites <- foldDraw $ \(Position pos, Sprite p) -> translateV2 pos $ p
    particles <- foldDraw $
        \(Particle _, Velocity (V2 vx vy), Position pos) ->
            translateV2 pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]

    cam <- get global
    Score s <- get global
    let score = color white . pictureOnHud cam . translateV2 scorePos . scale 0.1 0.1 . Text $ "Score: " ++ show s

    return $ sprites <> score <> particles

main :: IO ()
main = do
    content <- readFile "./src/meta.txt"
    let tileOptions = readTilesMeta content
        coords = createGrid 10 10
    grid <- (`doWaveCollapse` coords) $ createPreTileGrid tileOptions coords
    w <- initWorld
    runWith w $ do
        initialize grid coords
        play (InWindow "Haskill Issue" (220, 360) (10, 10)) black 60 draw preHandleEvent step

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
import Codec.Picture
import Control.Monad
import Linear (V2(..))
import qualified Linear as L
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Export.Image
import Graphics.Gloss.Interface.Environment

import Drawing.Sprites
import Drawing.Camera
import Misc
import Player
import Worlds
import Apecs.Extension
import Structure.Structure
import Grid.Implementation
import Grid.Tile
import qualified Data.Map as M
import Debug.Trace (trace)
import Debug.Time
import Enemy.Enemy
import Enemy.Pathfinding


makeWorld "World" [''Position, ''Velocity, ''Enemy, ''MovementPattern, ''MapGrid, ''Paths, ''PathFinder, ''Structure, ''Sprite, ''AnimatedSprite, ''Player, ''Particle, ''Score, ''Time, ''Inputs, ''Camera]
type AllComps = (Position, Enemy, Velocity, MovementPattern, PathFinder, Structure, Sprite, AnimatedSprite)

type SystemW a = System World a

xmin, xmax :: Float
xmin = -110
xmax = 110


playerPos :: V2 Float
playerPos = V2 0 0

initialize :: PathfindGraph -> Grid -> Int -> SystemW ()
initialize pathGraph grid size = do
    _playerEty <- newEntity (Player, Position playerPos, Velocity 0)
    modify global $ \(Camera pos _) -> Camera pos 1.6
    modify global $ \(Paths _ g) -> Paths pathGraph g
    modify global $ \(MapGrid _ _) -> MapGrid grid size
    _baseEty <- newEntity(Position (V2 0 0), Structure 200 [
        (96, 32), (96, -32),
        (-96, 32), (-96, -32),
        (32, 96), (32, -96),
        (-32, 96), (-32, -96)])
    updateGoals
    return ()

initialiseGrid :: (HasMany w [Position, Velocity, EntityCounter, Sprite]) => Grid -> [(Int,Int)] -> System w ()
initialiseGrid grid coords  = do
    let
        sprite = getGridSprite grid coords
        -- sprite =
    void $ newEntity (Position (V2 0 0), Sprite sprite)
    -- mapM_ void [newEntity (Position (V2 (64*fromIntegral x) (64*fromIntegral y)), Sprite $ pic (M.findWithDefault erTile (x,y) grid))| (x,y) <-coords]

clampPlayer :: SystemW ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) ->
    Position (V2 (min xmax . max xmin $ x) y)

destroyDeadStructures :: SystemW ()
destroyDeadStructures = do
    pathingChanged <- cfoldM (\b (Structure hp _, ety) -> do
        when (hp < 0) $ destroy ety (Proxy @AllComps )
        return (b || hp < 0)) False
    when pathingChanged $ do
        updateGoals
        clearPaths


step :: Float -> SystemW ()
step dT = do
    incrTime dT
    stepPosition dT
    animatedSprites dT
    stepParticles dT
    camOnPlayer
    doEnemy dT
    doPathFinding
    destroyDeadStructures
    triggerEvery dT 8 3 $  newEntity (Enemy 0 1 20, Position (V2 1400 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 1 $  newEntity (Enemy 0 1 20, Position (V2 1400 (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 5 $  newEntity (Enemy 0 1 20, Position (V2 (-1400) 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 7 $  newEntity (Enemy 0 1 20, Position (V2 (-1400) (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

draw :: Picture -> (Int,Int) -> SystemW Picture
draw bg (screenWidth, screenHeight) = do
    sprites <- foldDraw $ \(Position pos, Sprite p) -> translateV2 pos p
    particles <- foldDraw $
        \(Particle _, Velocity (V2 vx vy), Position pos) ->
            translateV2 pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]
    cam <- get global
    Score s <- get global
    let score = pictureOnHud cam (V2 (fromIntegral $ 30 - div screenWidth 2) (fromIntegral $ 50 - div screenHeight 2)) . scale 0.1 0.1 . color white .  Text $ "Base HP: " ++ show s
    return $  bg <> sprites <> particles <> score

main :: IO ()
main = do
    -- tempAudioMain
    content <- readFile "./src/meta.txt"
    let size = traceTimer "WFCollapse" 50
        tileOptions = readTilesMeta content
        graphicTileCoords = traceTimer "WFCollapse" createGrid size size
        pathFindCoords = map (tileCentre 2 . toRealCoord size) graphicTileCoords


    screenSize@(sWid,sHei) <- getScreenSize
    grid <- startTimer "WFCollapse" $ (`doWaveCollapse` graphicTileCoords) $ traceTimer "WFCollapse" $ collapseBaseGrid size $ traceTimer "WFCollapse" $ createPreTileGrid tileOptions graphicTileCoords
    background <- startTimer "GridImage" optimisePicturewithRes (sWid-100,sHei-100) (64*size,64*size) . translate 32 32 $ getGridSprite (traceTimer "GridImage" $ traceTimer "WFCollapse" grid) graphicTileCoords

    let getTile = tileOfCoord grid size
    
    w <- initWorld
    runWith w $ do

        startTimer "GraphCreation" $ initialize (traceTimer "GraphCreation" $ generateGraph (traceTimer "GraphCreation" getTile) pathFindCoords) grid size
        play FullScreen black 60 (draw (traceTimer "GridImage"  $ translate (fromIntegral $ -32*size) (fromIntegral $ -32*size) background) screenSize) handleInputs step

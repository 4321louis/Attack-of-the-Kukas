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

makeWorld "World" [ ''Position, ''Velocity, ''Enemy, ''MapGrid, ''Paths, 
                    ''PathFinder, ''Structure, ''Sprite, ''AnimatedSprite, ''Player,
                    ''Particle, ''Score, ''Time, ''Inputs, ''Camera, 
                    ''Hp, ''Seed, ''Plant, ''Craft]
type AllEnemyComps = (Position, Enemy, Velocity, PathFinder, Sprite, Hp)

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
    modify global $ \(Craft _) -> Craft []
    _seed <- newEntity(Position (V2 32 96), Sprite greenSeed, GreenSeed)
    _seed <- newEntity(Position (V2 96 (-32)), Sprite greenSeed, GreenSeed)
    _baseEty <- newEntity(Position (V2 0 0), Hp 200 0 0, Structure [
        V2 96 32, V2 96 (-32),
        V2 (-96) 32, V2 (-96) (-32),
        V2 32 96, V2 32 (-96),
        V2 (-32) 96, V2 (-32) (-96)])
    updateGoals
    return ()

initialiseGrid :: (HasMany w [Position, Velocity, EntityCounter, Sprite]) => Grid -> [(Int,Int)] -> System w ()
initialiseGrid grid coords  = do
    let
        sprite = getGridSprite grid coords
    void $ newEntity (Position (V2 0 0), Sprite sprite)

clampPlayer :: SystemW ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) ->
    Position (V2 (min xmax . max xmin $ x) y)

destroyDeadStructures :: SystemW ()
destroyDeadStructures = do
    pathingChanged <- cfoldM (\b (Structure _, Hp hp _ _ , ety) -> do
        when (hp <= 0) $ do 
            destroy ety (Proxy @AllPlantComps )
        return (b || hp <= 0)) False
    when pathingChanged $ do
        updateGoals
        clearPaths

destroyDeadEnemies :: SystemW ()
destroyDeadEnemies = do
    cmapM (\(Enemy _ _, Hp hp _ _ , ety) -> when (hp <= 0) $ destroy ety (Proxy @AllEnemyComps ))


step :: Float -> SystemW ()
step dT = do
    incrTime dT
    stepPosition dT
    animatedSprites dT
    stepParticles dT
    camOnPlayer
    doEnemy dT
    doPathFinding
    doPlants dT
    destroyDeadStructures
    destroyDeadEnemies
    triggerEvery dT 8 3 $  newEntity (Hp 100 100 0, Enemy 1 20, Position (V2 1400 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 1 $  newEntity (Hp 100 100 0, Enemy 1 20, Position (V2 1400 (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 5 $  newEntity (Hp 100 100 0, Enemy 1 20, Position (V2 (-1400) 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])
    triggerEvery dT 8 7 $  newEntity (Hp 100 100 0, Enemy 1 20, Position (V2 (-1400) (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder Nothing [])

draw :: Picture -> (Int,Int) -> SystemW Picture
draw bg (screenWidth, screenHeight) = do
    unsortedSprites <- cfold (\sprites (Position pos@(V2 _ y), Sprite p) -> (y,translateV2 pos p):sprites) []
    let sprites = foldMap snd $ sortWith (negate . fst) unsortedSprites
    particles <- foldDraw $
        \(Particle _, Velocity (V2 vx vy), Position pos) ->
            translateV2 pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]
    cam <- get global
    Score s <- get global
    let score = pictureOnHud cam (V2 (fromIntegral $ 30 - div screenWidth 2) (fromIntegral $ 50 - div screenHeight 2)) . scale 0.3 0.3 . color white .  Text $ "Base HP: " ++ show s
    return $  bg <> sprites <> particles <> score

main :: IO ()
main = do
    -- grid creatiion
    content <- readFile "./assets/Config/swampGeneration.txt"
    let size = traceTimer "WFCollapse" 50
        tileOptions = readTilesMeta content
        graphicTileCoords = traceTimer "WFCollapse" createGrid size size
    grid <- startTimer "WFCollapse" $ (`doWaveCollapse` graphicTileCoords) $ traceTimer "WFCollapse" $ collapseBaseGrid size $ traceTimer "WFCollapse" $ createPreTileGrid tileOptions graphicTileCoords
    
    -- background
    screenSize@(sWid,sHei) <- getScreenSize
    background <- startTimer "GridImage" optimisePicturewithRes (sWid-100,sHei-100) (64*size,64*size) . translate 32 32 $ getGridSprite (traceTimer "GridImage" $ traceTimer "WFCollapse" grid) graphicTileCoords
    
    --pathfinding setup
    let getTile = tileOfCoord grid size
        pathFindCoords = map (tileCentre 2 . toRealCoord size) graphicTileCoords
    
    tempAudioMain
    w <- initWorld
    runWith w $ do
        startTimer "GraphCreation" $ initialize (traceTimer "GraphCreation" $ generateGraph (traceTimer "GraphCreation" getTile) pathFindCoords) grid size
        play FullScreen black 60 (draw (traceTimer "GridImage"  $ translate (fromIntegral $ -32*size) (fromIntegral $ -32*size) background) screenSize) handleInputs step
    
    finishAudio

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
import Enemy.Hive
import Enemy.Enemy
import Enemy.Pathfinding
import Plant.Plant
import Plant.Seed
import Drawing.Sprites (spriteDir)
import Drawing.Hotbar

makeWorld "World" [ ''Position, ''Velocity, ''Enemy, ''MapGrid, ''Paths, 
                    ''PathFinder, ''Structure, ''Sprite, ''AnimatedSprite, ''Player,
                    ''Particle, ''Base, ''Time, ''Inputs, ''Camera, 
                    ''Hp, ''Seed, ''Plant, ''Inventory, ''DropHandler,
                    ''Hive ]

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
    modify global $ \(Inventory _ _) -> Inventory [0, 0, 0, 0] [RedSeed, RedSeed]
    _seed <- newEntity(Position (V2 32 96), Sprite greenSeed, GreenSeed)
    _seed <- newEntity(Position (V2 96 (-32)), Sprite redSeed, RedSeed)
    _seed <- newEntity(Position (V2 96 32), Sprite redSeed, RedSeed)
    _seed <- newEntity(Position (V2 (-96) (-32)), Sprite blueSeed, BlueSeed)
    _baseEty <- newEntity(Base, Position (V2 0 0), Hp 200 0 0, Structure [
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


step :: Float -> SystemW ()
step dT = do
    incrTime dT
    stepPosition dT
    animatedSprites dT
    camOnPlayer
    doEnemy dT
    doPathFinding
    doPlants dT
    destroyDeadStructures
    destroyDeadEnemies
    spawnEnemies dT

draw :: Picture -> (Int,Int) -> SystemW Picture
draw bg (screenWidth, screenHeight) = do
    unsortedSprites <- cfoldM (\sprites (Position pos@(V2 _ y), Sprite p, ety) -> do
        isSeed <- exists ety (Proxy @Seed)
        return $ (if isSeed then y-200 else y,translateV2 pos p):sprites) []
    let sprites = foldMap snd $ sortWith (negate . fst) unsortedSprites

    particles <- foldDraw $
        \(Particle _, Velocity (V2 vx vy), Position pos) ->
            translateV2 pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]
    cam <- get global
    Time time <- get global
    hp <- cfold (\a (Base, Hp hp _ _) -> hp) 0 
    
    Inventory inv _ <- get global
    let 
        hpPic = pictureOnHud cam (V2 (fromIntegral $ 30 - div screenWidth 2) (fromIntegral $ 50 - div screenHeight 2)) . scale 0.3 0.3 . color white .  Text $ "Base HP: " ++ show (ceiling hp)
        timeSpr = pictureOnHud cam (V2 (fromIntegral $ 30 - div screenWidth 2) (fromIntegral $ 100 - div screenHeight 2)) . scale 0.3 0.3 . color white .  Text $ "Time: " ++ show (floor $ time/60)  ++ ":"++ show (mod (floor time) 60 )
        hotbar = pictureOnHud cam (V2 (fromIntegral $ 80 - div screenWidth 2) (fromIntegral $ -350 + div screenHeight 2)) $ drawHotbar inv
    return $  bg <> sprites <> particles <> hotbar -- <> hpPic <> timeSpr

main :: IO ()
main = do
    -- grid creation
    content <- readFile "./assets/Config/swampGeneration.txt"
    let size = traceTimer "WFCollapse" 50
        tileOptions = readTilesMeta content
        graphicTileCoords = traceTimer "WFCollapse" createGrid size size
    (pregrid, bases) <- collapseStartingGrid size $ traceTimer "WFCollapse" $ createPreTileGrid tileOptions graphicTileCoords
    grid <- startTimer "WFCollapse" $ (`doWaveCollapse` graphicTileCoords) $ traceTimer "WFCollapse" pregrid

    -- background
    screenSize@(sWid,sHei) <- getScreenSize
    background <- startTimer "GridImage" optimisePicturewithRes (sWid-100,sHei-100) (64*size,64*size) . translate 32 32 $ getGridSprite (traceTimer "GridImage" $ traceTimer "WFCollapse" grid) graphicTileCoords
    
    --pathfinding setup
    let getTile = tileOfCoord grid size
        pathFindCoords = map (tileCentre 2 . toRealCoord size) graphicTileCoords
    
    initializeAudio
    w <- initWorld
    runWith w $ do
        startTimer "GraphCreation" $ initialize (traceTimer "GraphCreation" $ generateGraph (traceTimer "GraphCreation" getTile) pathFindCoords) grid size
        initializeHives size bases
        play FullScreen black 60 (draw (traceTimer "GridImage"  $ translate (fromIntegral $ -32*size) (fromIntegral $ -32*size) background) screenSize) handleInputs step
    
    finishAudio

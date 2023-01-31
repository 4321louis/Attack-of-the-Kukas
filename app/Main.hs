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
import Graphics.Gloss.Game hiding (play)
import Graphics.Gloss.Export.Image
import Codec.Picture

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
import Enemy.Pathfinding

import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent

import Sound.ProteaAudio


makeWorld "World" [''Position, ''Velocity, ''MovementPattern, ''Paths, ''PathFinder, ''Structure, ''Sprite, ''AnimatedSprite, ''Player, ''Bullet, ''Particle, ''Score, ''Time, ''Inputs, ''Camera]


type SystemW a = System World a

xmin, xmax :: Float
xmin = -110
xmax = 110

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 0
scorePos = V2 xmin (-170)

initialize :: PathfindGraph -> SystemW ()
initialize pathGraph = do
    _playerEty <- newEntity (Player, Position playerPos, Velocity 0)
    modify global $ \(Camera pos _) -> Camera pos 1.6
    modify global $ \(Paths _ g) -> Paths pathGraph g
    _baseEty <- newEntity(Structure 200 [   
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

clearBullets :: SystemW ()
clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
    if y > 170
        then Right (Not @(Bullet, Kinetic), Score (s - missPenalty))
        else Left ()

-- handleCollisions :: SystemW ()
-- handleCollisions =
--     cmapM_ $ \(Target, Position posT, etyT) ->
--         cmapM_ $ \(Bullet, Position posB, etyB) ->
--             when (L.norm (posT - posB) < 10) $ do
--                 destroy etyT (Proxy @(Target, Kinetic))
--                 destroy etyB (Proxy @(Bullet, Kinetic))
--                 spawnParticles 15 (Position posB) (-500, 500) (200, -50)
--                 modify global $ \(Score x) -> Score (x + hitBonus)
--                 modify global $ \(Camera pos cScale) -> Camera pos (0.85*cScale)

step :: Float -> SystemW ()
step dT = do
    incrTime dT
    stepPosition dT
    animatedSprites dT
    stepParticles dT
    camOnPlayer
    doPathFinding
    triggerEvery dT 8 3 $  newEntity (Position (V2 1400 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder (Just [(0,0)]) [])
    triggerEvery dT 8 1 $  newEntity (Position (V2 1400 (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder (Just [(0,0)]) [])
    triggerEvery dT 8 5 $  newEntity (Position (V2 (-1400) 1400), Sprite targetSprite2, Velocity (V2 0 0), PathFinder (Just [(0,0)]) [])
    triggerEvery dT 8 7 $  newEntity (Position (V2 (-1400) (-1400)), Sprite targetSprite2, Velocity (V2 0 0), PathFinder (Just [(0,0)]) [])

updateGoals :: SystemW ()
updateGoals = do
    modify global $ \(Paths graph _) -> Paths graph []
    cmapM_ $ \(Structure _ points) -> do modify global $ \(Paths graph goals) -> Paths graph (points ++ goals)

draw :: Picture -> SystemW Picture
draw bg = do
    sprites <- foldDraw $ \(Position pos, Sprite p) -> translateV2 pos p
    particles <- foldDraw $
        \(Particle _, Velocity (V2 vx vy), Position pos) ->
            translateV2 pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]

    cam <- get global
    Score s <- get global
    let score = color white . pictureOnHud cam . translateV2 scorePos . scale 0.1 0.1 . Text $ "Score: " ++ show s

    return $ bg <> sprites <> score <> particles

waitPlayback = do
    n <- soundActiveAll
    when  (n > 0) $ do
        threadDelay oneSec
        waitPlayback

oneSec :: Int
oneSec = 1000000 -- micro seconds

tempAudioMain = do
    let filename = "./src/menuLoop.wav" 

    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    -- (A) load sample from file
    sampleA <- sampleFromFile filename 1.0 -- volume

    -- start two sound tracks with shared sample data
    sndTrkA <- soundPlay sampleA 1 0 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    threadDelay oneSec -- wait 1 sec
    sndTrkB <- soundPlay sampleA 0 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    soundActive sndTrkB >>= print
    -- play 3 sec
    threadDelay $ 3 * oneSec
    soundStop sndTrkB
    soundActive sndTrkB >>= print
    -- wait sndTrkA to finish
    waitPlayback

    -- (B) load from memory buffer
    buffer <- SB.readFile filename
    sampleB <- case takeExtension filename of
        ".ogg" -> sampleFromMemoryOgg buffer 1.0
        ".wav" -> sampleFromMemoryWav buffer 1.0

    soundPlay sampleB 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    waitPlayback

    sampleDestroy sampleB
    soundPlay sampleB 1 1 0 1 -- we have invalidated the handle; nothing should happen now
    waitPlayback

    finishAudio

main :: IO ()
main = do
    -- tempAudioMain
    content <- readFile "./src/meta.txt"
    let size = traceTimer "WFCollapse" 50
        tileOptions = readTilesMeta content
        graphicTileCoords = traceTimer "WFCollapse" createGrid size size
        pathFindCoords = map (toRealCoord size) graphicTileCoords
        
    
    grid <- startTimer "WFCollapse" $ (`doWaveCollapse` graphicTileCoords) $ traceTimer "WFCollapse" $ collapseBaseGrid size $ traceTimer "WFCollapse" $ createPreTileGrid tileOptions graphicTileCoords
    background <- startTimer "GridImage" optimisePicture (64*size,64*size) . translate 32 32 $ getGridSprite (traceTimer "GridImage" $ traceTimer "WFCollapse" grid) graphicTileCoords
    
    let getTile = tileOfCoord grid size
    
    w <- initWorld
    runWith w $ do
        
        startTimer "GraphCreation" $ initialize (traceTimer "GraphCreation" $ generateGraph (traceTimer "GraphCreation" getTile) pathFindCoords)
        play (InWindow "Haskill Issue" (1280, 720) (10, 10)) black 60 (draw (traceTimer "GridImage"  $ translate (fromIntegral $ -32*size) (fromIntegral $ -32*size) background)) handleInputs step

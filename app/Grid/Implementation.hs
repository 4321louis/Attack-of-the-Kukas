-- More specific details for how the grid is handled with the rest of the game

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}


module Grid.Implementation where

import Drawing.Sprites
import Apecs.Gloss
import Graphics.Gloss.Game hiding (line)
import Debug.Trace
import Grid.Tile
import Data.Either
import qualified Data.Map as M
import Misc


toRealCoord :: Int -> (Int,Int) -> (Float,Float)
toRealCoord size (x,y) = (fromIntegral (64*x-32*size),fromIntegral ( 64*y-32*size)) 

fromRealCoord :: Int -> (Float,Float) -> (Int,Int)
fromRealCoord size (x,y) = (floor  $ (x+32*fromIntegral size)/64 , floor $ (y+32*fromIntegral size)/64 )

tileOfCoord :: Grid -> Int -> (Float,Float) -> Maybe Tile
tileOfCoord grid size (x,y) = M.lookup (fromRealCoord size (x,y)) grid

tileCentre :: Int -> (Float, Float) -> (Float, Float)
tileCentre size (x,y) = if even size then (32 + fromIntegral (floorMultiple x 64),32 + fromIntegral (floorMultiple y 64)) else (fromIntegral (floorMultiple (x-32) 64),fromIntegral (floorMultiple (y-32) 64))

getGridSprite :: Grid -> [(Int, Int)] -> Picture
getGridSprite grid coords = foldr (<>) Blank [translate (64*fromIntegral x) (64*fromIntegral y) $ pic (M.findWithDefault erTile (x,y) grid)| (x,y) <-coords]

collapseBaseGrid :: Int -> PreGrid -> PreGrid
collapseBaseGrid size = let
        doInit c t g = propegateCell c $ M.insert c (Right t) g
        landTile spr = Tile spr Land Land Land Land False False
        enemyTile = Tile (png $ spriteDir ++ "Terrain/Water.png") Land Land Land Land True False
        center = div size 2
        in doInit (center-1,center) (landTile $ png $ spriteDir ++  "Terrain/Base1.png") . doInit (center,center) (landTile $ png $ spriteDir ++  "Terrain/Base2.png") .
            doInit (center-1,center-1) (landTile $ png $ spriteDir ++  "Terrain/Base3.png") . doInit (center,center-1) (landTile $ png $ spriteDir ++  "Terrain/Base4.png") . 
            if size == 50 then doInit (46,46) enemyTile . doInit (3,46) enemyTile . 
                doInit (46,3) enemyTile . doInit (3,3) enemyTile else id

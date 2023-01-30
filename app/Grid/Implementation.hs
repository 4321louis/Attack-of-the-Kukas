

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


toRealCoord :: Int -> (Int,Int) -> (Float,Float)
toRealCoord size (x,y) = (32 + fromIntegral (64*x-32*size),32 + fromIntegral ( 64*y-32*size)) 

fromRealCoord :: Int -> (Float,Float) -> (Int,Int)
fromRealCoord size (x,y) = (floor  $ (x-32+32*fromIntegral size)/64 , floor $ (y-32+32*fromIntegral size)/64 )

tileOfCoord :: Grid -> Int -> (Float,Float) -> Maybe Tile
tileOfCoord grid size (x,y) = M.lookup (fromRealCoord size (x,y)) grid


getGridSprite :: Grid -> [(Int, Int)] -> Picture
getGridSprite grid coords = foldr (<>) Blank [translate (64*fromIntegral x) (64*fromIntegral y) $ pic (M.findWithDefault erTile (x,y) grid)| (x,y) <-coords]

collapseBaseGrid :: Int -> PreGrid -> PreGrid
collapseBaseGrid size = let
        doInit c t g = propegateCell c $ M.insert c (Right t) g
        landTile spr = Tile spr Land Land Land Land True True
        center = div size 2
        in doInit (center-1,center) (landTile $ png "./src/Base1.png") . doInit (center,center) (landTile $ png "./src/Base2.png") .
            doInit (center-1,center-1) (landTile $ png "./src/Base3.png") . doInit (center,center-1) (landTile $ png "./src/Base4.png")

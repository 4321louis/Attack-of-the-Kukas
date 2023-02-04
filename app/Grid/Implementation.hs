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
import Linear (V2(..))
import System.Random (randomRIO, randomIO)


toRealCoord :: Int -> (Int,Int) -> V2 Float
toRealCoord size (x,y) = V2 (fromIntegral (64*x-32*size)) (fromIntegral ( 64*y-32*size))

fromRealCoord :: Int -> V2 Float -> (Int,Int)
fromRealCoord size (V2 x y) = (floor  $ (x+32*fromIntegral size)/64 , floor $ (y+32*fromIntegral size)/64 )

tileOfCoord :: Grid -> Int -> V2 Float -> Maybe Tile
tileOfCoord grid size coord = M.lookup (fromRealCoord size coord) grid

tileCentre :: Int -> V2 Float -> V2 Float
tileCentre size (V2 x y) = if even size then V2 (32 + fromIntegral (floorMultiple x 64)) (32 + fromIntegral (floorMultiple y 64)) else V2 (fromIntegral (floorMultiple (x-32) 64)) (fromIntegral (floorMultiple (y-32) 64))

getGridSprite :: Grid -> [(Int, Int)] -> Picture
getGridSprite grid coords = foldr (<>) Blank [translate (64*fromIntegral x) (64*fromIntegral y) $ pic (M.findWithDefault erTile (x,y) grid)| (x,y) <-coords]

collapseStartingGrid :: Int -> PreGrid -> IO (PreGrid,[(Int,Int)])
collapseStartingGrid size grid = do
        let center = div size 2
            bugRange = randomRIO (4,46)
        b1 <- bugRange
        b2 <- bugRange
        b3 <- bugRange
        b4 <- bugRange
        b5 <- bugRange
        side <- randomRIO (0,3)
        let prebugBases = [(b1,47),(b2,3),(47,b3),(3,b4)]
            bugBases = placeLastBase b5 side prebugBases:prebugBases
        return (collapseLand center bugBases . collapseBugBase bugBases . collapseBase center $ grid, bugBases)

collapseBase :: Int -> PreGrid -> PreGrid
collapseBase center = let baseTile spr = Tile spr Land Land Land Land False False in
    intialiseCell (center-1,center) (baseTile $ png $ spriteDir ++  "Terrain/Base1.png") .
    intialiseCell (center,center) (baseTile $ png $ spriteDir ++  "Terrain/Base2.png") .
    intialiseCell (center-1,center-1) (baseTile $ png $ spriteDir ++  "Terrain/Base3.png") .
    intialiseCell (center,center-1) (baseTile $ png $ spriteDir ++  "Terrain/Base4.png")

collapseLand :: Foldable t => Int -> t (Int, Int) -> PreGrid -> PreGrid
collapseLand center bugbases grid  =
    let landTile = Tile (png $ spriteDir ++ "Terrain/Land.png") Land Land Land Land True True
        outerRingCoords = [(3,3),(3,47),(47,3),(47,47)] ++ concat [[(c, 47), (c, 3), (47, c),(3, c)] | c <- [4..46]]
        allCoords = [(center+1,center),(center+1,center-1),(center-2,center-1),(center,center+1)] ++ filter (not . (`elem` bugbases)) outerRingCoords
    in foldr (`intialiseCell` landTile) grid allCoords

collapseBugBase :: Foldable t => t (Int, Int) -> PreGrid -> PreGrid
collapseBugBase bugBases grid =
    let bugBaseTile = Tile (png $ spriteDir ++ "Terrain/KukasBase.png") Land Land Land Land True False
    in foldr (`intialiseCell` bugBaseTile) grid bugBases

placeLastBase :: Int -> Int -> [(Int,Int)] -> (Int,Int)
placeLastBase pos side bugBases =
    let coord = case side of
            0 -> (47,pos)
            1 -> (3,pos)
            2 -> (pos,47)
            3 -> (pos,3)
            _ -> (47,47)
    in if coord `elem` bugBases then (47,47) else coord



intialiseCell :: (Int, Int) -> Tile -> PreGrid -> PreGrid
intialiseCell c t g = propegateCell c $ M.insert c (Right t) g
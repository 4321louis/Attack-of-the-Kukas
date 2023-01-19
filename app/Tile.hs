-- Tiles and Procedural Generation 

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
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tile where

import Drawing.Sprites
import Apecs.Gloss
import Graphics.Gloss.Game hiding (line)
import qualified Data.Map as M
import Data.Maybe
import Data.List ( sortBy )
import Data.Either
import Misc (atRandIndex)

data Side = Water | LWater | RWater | Land deriving (Show,Read)
data Tile = Tile Picture Side Side Side Side deriving (Show)
-- TODO:HitBoxes grid
-- instance Component Tile where type Storage Tile = Map Tile

readTilesMeta :: String -> [Tile]
readTilesMeta content =
    let
        tileLines = lines content
        readTile l =  
            let [name,doRotate,sn,se,ss,sw] = words l
                [n,e,s,w] :: [Side] = read <$> [sn,se,ss,sw]
                img = png $ "./src/" ++ name
            in
                (if read doRotate then id else take 1) 
                    [   Tile img n e s w, 
                        Tile (rotate 90 img) w n e s, 
                        Tile (rotate 180 img) s w n e, 
                        Tile (rotate 270 img) e s w n]
    in concatMap readTile tileLines

connects :: Side -> Side
connects Water = Water
connects Land = Land
connects LWater = RWater
connects RWater = LWater

erTile :: Tile
erTile = Tile targetSprite2 Water Water Water Water

createGrid:: Int -> Int -> [(Int,Int)]
createGrid x y = [(xs,ys)| xs<-map (*64) [0..x-1], ys<-map (*64) [0..y-1]]

createPreTileGrid :: [Tile] -> [(Int,Int)] -> PreGrid
createPreTileGrid tileOptions = foldr (`M.insert` Left tileOptions) M.empty

type PreGrid = M.Map (Int,Int) (Either [Tile] Tile)
type Grid = M.Map (Int,Int) Tile

getPic :: Tile -> Picture
getPic (Tile img _ _ _ _) = img

doWaveCollapse :: PreGrid -> [(Int,Int)] -> IO Grid
doWaveCollapse grid coords = do
    let 
        byEntropy = sortBy (\c1 c2 -> compareEntropy (fromJust $ M.lookup c1 grid) (fromJust $ M.lookup c2 grid)) coords
    nextGrid <- collapseCell (head byEntropy) grid
    if isLeft . fromJust $ M.lookup (head byEntropy) grid 
    then doWaveCollapse nextGrid coords
    else return $ foldr (\k -> M.insert k (fromRight erTile . fromJust $ M.lookup k grid)) M.empty coords 

compareEntropy :: Foldable t => Either (t a) b -> Either (t a) b -> Ordering
compareEntropy o1 o2 = 
    let
        [l1,l2] = either length (const 1) <$> [o1,o2] 
    in if l1 == l2 then EQ else if l1 > l2 then LT else GT

collapseCell :: (Int,Int) -> PreGrid -> IO PreGrid
collapseCell cell grid = do
    tile <- atRandIndex . fromLeft [erTile] . fromJust $ M.lookup cell grid
    return $ M.insert cell (Right tile) grid
-- TODO:propegate the collapse to adjacent tiles

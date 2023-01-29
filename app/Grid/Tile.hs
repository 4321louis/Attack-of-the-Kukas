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

module Grid.Tile where

import Drawing.Sprites
import Apecs.Gloss
import Graphics.Gloss.Game hiding (line)
import qualified Data.Map as M
import Data.Maybe
import Data.List ( sortBy, minimumBy )
import Data.Either
import Misc (atRandIndex, concatRep)
import qualified Data.Vector as V

data Side = Water | LWater | RWater | MWater | Land deriving (Show,Read,Eq)
data Tile = Tile 
    { pic :: Picture
    , north :: Side 
    , east :: Side
    , south :: Side
    , west :: Side
    , walkable :: Bool
    , placeable :: Bool} deriving (Show,Eq)
-- TODO:HitBoxes grid


connects :: Side -> Side -> Bool
connects Water Water = True
connects Land Land = True
connects LWater RWater = True
connects RWater LWater = True
connects MWater MWater = True
connects _ _ = False

erTile, erTile2, erTile3 :: Tile
erTile = Tile targetSprite4 Water Water Water Water False False
erTile2 = Tile targetSprite2 Water Water Water Water False False
erTile3 = Tile targetSprite3 Water Water Water Water False False

readTilesMeta :: String -> V.Vector Tile
readTilesMeta content =
    let
        tileLines = lines content
        readTile l = if l == "" then [] else
            let [name,count,doRotate,sn,se,ss,sw,walk,place] = words l
                [n,e,s,w] :: [Side] = read <$> [sn,se,ss,sw]
                img = png $ "./src/" ++ name
            in
                concatRep (read count) $ (if read doRotate then id else take 1)
                    [   Tile img n e s w (read walk) (read place),
                        Tile (rotate 90 img) w n e s (read walk) (read place),
                        Tile (rotate 180 img) s w n e (read walk) (read place),
                        Tile (rotate 270 img) e s w n (read walk) (read place)]
    in V.fromList $ concatMap readTile tileLines

createGrid:: Int -> Int -> [(Int,Int)]
createGrid x y = [(xs,ys)| xs<-[0..x-1], ys<-[0..y-1]]

createPreTileGrid :: V.Vector Tile -> [(Int,Int)] -> PreGrid
createPreTileGrid tileOptions = foldr (`M.insert` Left tileOptions) M.empty

type PreGrid = M.Map (Int,Int) (Either (V.Vector Tile) Tile)
type Grid = M.Map (Int,Int) Tile

doWaveCollapse :: PreGrid -> [(Int,Int)] -> IO Grid
doWaveCollapse grid coords = do
    let
        lowestEntropy = minimumBy (\c1 c2 -> compareEntropy (fromJust $ M.lookup c1 grid) (fromJust $ M.lookup c2 grid)) coords
    nextGrid <- collapseCell lowestEntropy grid
    if isLeft . fromJust $ M.lookup lowestEntropy grid
    then doWaveCollapse nextGrid coords
    else return $ foldr (\k -> M.insert k (fromRight erTile . fromJust $ M.lookup k grid)) M.empty coords

compareEntropy :: Either (V.Vector a) b -> Either (V.Vector a) b -> Ordering
compareEntropy o1 o2 =
    let
        [l1,l2] = either V.length (const 1000) <$> [o1,o2]
    in if l1 == l2 then EQ else if l1 < l2 then LT else GT

collapseCell :: (Int,Int) -> PreGrid -> IO PreGrid
collapseCell cell grid = do
    tile <- atRandIndex . fromLeft (V.singleton erTile2) . fromJust $ M.lookup cell grid
    let chosenGrid = M.insert cell (Right tile) grid
    return $ propegateCell cell chosenGrid

propegateCell :: (Int,Int) -> PreGrid -> PreGrid
propegateCell coord@(x,y) grid =
    let
        focus = either id V.singleton . fromJust $ M.lookup coord grid
        doCell c constraint cgrid = let (changed, ngrid) = constrainCell constraint c cgrid in if changed then propegateCell c ngrid else cgrid
        correctTouch d1 d2 other = V.any (\option -> connects (d1 option) (d2 other)) focus
    in foldr ($) grid (zipWith ($) (doCell <$> [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]) [correctTouch west east, correctTouch east west,  correctTouch north south, correctTouch south north] ) 


constrainCell :: (Tile -> Bool) -> (Int,Int) -> PreGrid -> (Bool,PreGrid)
constrainCell constaint coord grid =
    let oldCell = M.lookup coord grid 
        newCell = either (Left . V.filter constaint) Right . fromJust $ oldCell
    in if isNothing oldCell || Just newCell == oldCell then (False, grid) else
    (True, M.insert coord newCell grid)

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
import Data.List (nub, sortBy, minimumBy, delete )
import Data.Either
import Misc (atRandIndex, concatRep)
import Debug.Trace
import qualified Data.Vector as V

data Side = Water | LWater | RWater | Land deriving (Show,Read,Eq)
data Tile = Tile 
    { pic :: Picture
    , north :: Side 
    , east :: Side
    , south :: Side
    , west :: Side} deriving (Show,Eq)
-- TODO:HitBoxes grid


connects :: Side -> Side -> Bool
connects Water Water = True
connects Land Land = True
connects LWater RWater = True
connects RWater LWater = True
connects _ _ = False
-- instance Component Tile where type Storage Tile = Map Tile

erTile, erTile2, erTile3 :: Tile
erTile = Tile targetSprite4 Water Water Water Water
erTile2 = Tile targetSprite2 Water Water Water Water
erTile3 = Tile targetSprite3 Water Water Water Water

readTilesMeta :: String -> V.Vector Tile
readTilesMeta content =
    let
        tileLines = lines content
        readTile l =
            let [name,count,doRotate,sn,se,ss,sw] = words l
                [n,e,s,w] :: [Side] = read <$> [sn,se,ss,sw]
                img = png $ "./src/" ++ name
            in
                concatRep (read count) $ (if read doRotate then id else take 1)
                    [   Tile img n e s w,
                        Tile (rotate 90 img) w n e s,
                        Tile (rotate 180 img) s w n e,
                        Tile (rotate 270 img) e s w n]
    in V.fromList $ concatMap readTile tileLines

createGrid:: Int -> Int -> [(Int,Int)]
createGrid x y = [(xs,ys)| xs<-[0..x-1], ys<-[0..y-1]]

createPreTileGrid :: V.Vector Tile -> [(Int,Int)] -> PreGrid
createPreTileGrid tileOptions = foldr (`M.insert` Left tileOptions) M.empty

type PreGrid = M.Map (Int,Int) (Either (V.Vector Tile) Tile)
type Grid = M.Map (Int,Int) Tile

highEntropy :: Int
highEntropy = 100000

doWaveCollapse :: PreGrid -> [(Int,Int)] -> IO Grid
doWaveCollapse grid coords = 
    doWaveCollapseLoop grid $ sortBy (compareEntropy grid) coords 


doWaveCollapseLoop :: PreGrid -> [(Int,Int)] -> IO Grid
doWaveCollapseLoop grid (h:t) = do
        (changed, nextGrid) <- collapseCell h grid
        doWaveCollapseLoop nextGrid $ entropySortBy (compareEntropy nextGrid) (nub changed) t

doWaveCollapseLoop grid [] = return $ foldr (\k -> M.insert k (fromRight erTile . fromJust $ M.lookup k grid)) M.empty (M.keys grid)

compareEntropy :: PreGrid -> (Int,Int) -> (Int,Int) -> Ordering
compareEntropy grid o1 o2 =
    let
        [l1,l2] = preTileEntropy . fromJust . (`M.lookup` grid) <$> [o1,o2]
    in if l1 < l2 then LT else if l1 == l2 then EQ else  GT

preTileEntropy :: Either (V.Vector a) b -> Int
preTileEntropy = either V.length (const highEntropy)

collapseCell :: (Int,Int) -> PreGrid -> IO ([(Int,Int)],PreGrid)
collapseCell cell grid = do
    tile <- atRandIndex . fromLeft (V.singleton erTile2) . fromJust $ M.lookup cell grid
    let chosenGrid = M.insert cell (Right tile) grid
    return $ propegateCell cell chosenGrid

propegateCell :: (Int,Int) -> PreGrid -> ([(Int,Int)],PreGrid)
propegateCell coord@(x,y) grid =
    let
        focus = either id V.singleton . fromJust $ M.lookup coord grid
        doCell c constraint cgrid = let 
            (changed, ngrid) = constrainCell constraint c cgrid 
            (rest, fgrid) = propegateCell c ngrid
            in if changed then (c:rest,fgrid) else ([],cgrid)
        correctTouch d1 d2 other = V.any (\option -> connects (d1 option) (d2 other)) focus
    in foldr (\func (changed0, grid0)-> let (changed1,grid1) = func grid0 in (changed1++changed0,grid1) ) ([],grid) (zipWith ($) (doCell <$> [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]) [correctTouch west east, correctTouch east west,  correctTouch north south, correctTouch south north] ) 


constrainCell :: (Tile -> Bool) -> (Int,Int) -> PreGrid -> (Bool,PreGrid)
constrainCell constaint coord grid =
    let oldCell = M.lookup coord grid 
        newCell = either (Left . V.filter constaint) Right . fromJust $ oldCell
    in if isNothing oldCell || Just newCell == oldCell then (False, grid) else
    (True, M.insert coord newCell grid)

entropySortBy :: Eq a => (a -> a -> Ordering) -> [a] -> [a] -> [a]
entropySortBy _ [] list = list
entropySortBy comp (c:cs) list = 
    let (left,right) = seperate c list
    in entropySortBy comp cs $ insert comp c left ++ right

seperate :: Eq a => a -> [a] -> ([a], [a]) 
seperate e (x:xs) 
    | e == x = ([],xs)
    | otherwise = let (left,right) = seperate e xs in (x:left,right)
seperate _ [] = ([],[])

insert :: (a -> a -> Ordering) -> a -> [a] -> [a]
insert _ x [] = [x]
insert comp x (y:ys) = if LT == comp x y 
    then x:y:ys 
    else y : insert comp x ys
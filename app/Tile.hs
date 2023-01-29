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
import qualified Data.Massiv.Array as Msv hiding (take)
import Data.Massiv.Array hiding (take, zipWith, read)
import GHC.IO (unsafePerformIO)
import Control.Monad (foldM)

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

createGrid:: Int -> Int -> [Ix2]
createGrid x y = [Ix2 xs ys| xs<-[0..x-1], ys<-[0..y-1]]

createPreTileGrid :: Int -> Int -> V.Vector Tile -> IO (PreGrid) 
createPreTileGrid x y = newMArray (Sz (x :. y))

-- class (Load B Ix2 (V.Vector Tile), Manifest B (V.Vector Tile), Manifest B Tile) => GridConstraint

type PreGrid = MArray (PrimState IO) B Ix2 (V.Vector Tile)
type Grid = Array B Ix2 Tile

highEntropy :: Int
highEntropy = 100000

doWaveCollapse :: PreGrid -> [Ix2] -> IO (Grid)
doWaveCollapse grid coords = 
    doWaveCollapseLoop grid $ sortBy (\c1 c2 ->unsafePerformIO $ compareEntropy grid c1 c2) coords 


doWaveCollapseLoop :: PreGrid -> [Ix2] -> IO (Grid)
doWaveCollapseLoop grid (h:t) = do
        changed <- collapseCell h grid
        doWaveCollapseLoop grid $ entropySortBy (compareEntropy grid) (nub changed) t
doWaveCollapseLoop grid [] = do
    frozen <- freezeS grid
    Data.Massiv.Array.mapM (return . V.head) frozen

compareEntropy :: PreGrid -> Ix2 -> Ix2 -> IO Ordering
compareEntropy grid o1 o2 = do 
    c1 <- readM grid o1
    c2 <- readM grid o2
    let
        l1 = preTileEntropy c1
        l2 = preTileEntropy c2
    return $ if l1 < l2 then LT else if l1 == l2 then EQ else  GT

preTileEntropy :: V.Vector a -> Int
preTileEntropy v = let l = V.length v in if l == 1 then highEntropy else l

collapseCell :: Ix2 -> PreGrid -> IO [Ix2]
collapseCell cell grid = do
    cells <- readM grid cell
    tile <- atRandIndex cells 
    _ <- write grid cell (V.singleton tile)
    propegateCell cell grid

propegateCell :: Ix2 -> PreGrid -> IO [Ix2]
propegateCell coord@(Ix2 x y) grid = do 
    focus <- Msv.read grid coord
    let correctTouch d1 d2 other = V.any (\option -> connects (d1 option) (d2 other)) (fromJust focus)
        doCell c constraint grid  = do 
            changed <- constrainCell constraint c grid
            
            if changed then do 
                rest <- propegateCell c grid
                return $ c:rest 
            else return []
    foldM (\changed0 func -> do 
        changed1 <- func grid
        return (changed1++changed0)) [] (zipWith ($) (doCell <$> [Ix2 (x-1) y,Ix2 (x+1) y,Ix2 x (y+1),Ix2 x (y-1)]) [correctTouch west east, correctTouch east west,  correctTouch north south, correctTouch south north] ) 


constrainCell :: (Tile -> Bool) -> Ix2 -> PreGrid -> IO Bool
constrainCell constaint coord grid = do 
    oldCell <- Msv.read grid coord
    let newCell = V.filter constaint . fromJust $ oldCell
    if isNothing oldCell || V.length newCell == (V.length . fromJust $ oldCell) then return False else
        do  
            writeM grid coord newCell 
            return True

entropySortBy :: Eq a => (a -> a -> IO Ordering) -> [a] -> [a] -> [a]
entropySortBy _ [] list = list
entropySortBy comp (c:cs) list = 
    let (left,right) = seperate c list
    in entropySortBy comp cs $ insert (\c1 c2 -> unsafePerformIO $ comp c1 c2) c left ++ right

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
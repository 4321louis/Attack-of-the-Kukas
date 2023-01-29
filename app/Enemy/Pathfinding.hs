-- Helps enemies find where they're going

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Enemy.Pathfinding where
import Data.Graph.AStar
import qualified Data.Map as M
import Data.Maybe
import Grid.Tile (Tile(walkable), Grid, erTile)
import qualified Data.HashSet as HS
import qualified Data.Set as S
import Apecs.Gloss (Key)
import Linear (V2(..))
import Apecs
import Misc
import Apecs.Extension
import Apecs.Physics
import qualified Linear as L

type PathfindGraph = M.Map (Int,Int) (HS.HashSet (Int,Int))

newtype Paths = Paths PathfindGraph deriving (Show)
instance Semigroup Paths where (Paths a) <> (Paths b) = Paths (M.union a b)
instance Monoid Paths where mempty = Paths M.empty
instance Component Paths where type Storage Paths = Global Paths

data PathFinder = PathFinder (Maybe [(Int,Int)]) [(Int,Int)] deriving (Show)
instance Component PathFinder where type Storage PathFinder = Map PathFinder

acquireNewPaths :: (HasMany w [PathFinder, Position, Paths]) => System w ()
acquireNewPaths = cmapM $ \(p@(PathFinder (Just goals) []), Position (V2 x y)) -> do
    Paths pathscape <- get global
    let path  = findPathToClosest pathscape goals (floorMultiple 64 x,floorMultiple 64 y)
    return $ maybe p (PathFinder (Just goals)) path

moveOnPath :: (HasMany w [PathFinder, Velocity, Position]) => System w ()
moveOnPath = cmap $ \(p@(PathFinder goals (node@(nx,ny):rest)), Position pos@(V2 px py), v@(Velocity _)) ->
    if sqDistance (fromIntegral nx,fromIntegral ny) (px,py) < 16 
        then (PathFinder goals rest, if null rest then Velocity (V2 0 0) else Velocity ((L.^* 2) . L.normalize $ V2 (fromIntegral nx) (fromIntegral ny) - pos)) 
        else (p,v)


findPathToClosest grid goals = aStar
    (fromJust . (`M.lookup` grid))
    sqDistance
    (\p -> minimum $ map (sqDistance p) goals)
    (\loc -> any ((<64) . sqDistance loc) goals)

sqDistance :: Num a => (a, a) -> (a, a) -> a
sqDistance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2



generateGraph :: Grid -> [(Int,Int)] -> PathfindGraph
generateGraph grid coords = M.fromList $ map (\n -> (n,HS.fromList (findAdjacent grid n))) coords


findAdjacent :: Grid -> (Int,Int) -> [(Int,Int)]
findAdjacent grid target@(x,y) = filter (moveable grid target) [(x - 32, y),(x + 32, y),(x, y + 32),(x, y - 32),(x - 32, y - 32),(x + 32, y + 32),(x + 32, y + 32),(x - 32, y - 32)]

moveable :: Grid -> (Int, Int) -> (Int, Int) -> Bool
moveable grid p1 p2 =  isWalkable p1 && isWalkable p2
    where isWalkable (x,y) = walkable . fromJust $ M.lookup (floorMultiple 64 (fromIntegral x),floorMultiple 64 (fromIntegral y)) grid

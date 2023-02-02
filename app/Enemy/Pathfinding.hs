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
import Worlds
import Debug.Trace
import qualified Linear as L
import Grid.Tile
import Grid.Implementation

type PathfindGraph = M.Map (V2 Float) (HS.HashSet (V2 Float))

data Paths = Paths PathfindGraph [V2 Float] deriving (Show)
instance Semigroup Paths where (Paths a g1) <> (Paths b g2) = Paths (M.union a b) (g1 ++ g2)
instance Monoid Paths where mempty = Paths M.empty []
instance Component Paths where type Storage Paths = Global Paths

data PathFinder = PathFinder (Maybe [V2 Float]) [V2 Float] deriving (Show)
instance Component PathFinder where type Storage PathFinder = Map PathFinder

doPathFinding :: (HasMany w [PathFinder, Paths, Position, Velocity, MapGrid]) => System w ()
doPathFinding = do
    acquireNewPaths
    checkGoal

clearPaths :: (Has w IO PathFinder) => System w ()
clearPaths =  cmap $ \(PathFinder mGoals _) -> PathFinder mGoals []

acquireNewPaths :: (HasMany w [PathFinder, Paths, Position, MapGrid]) => System w ()
acquireNewPaths = cmapM $ \(p@(PathFinder mGoals cpath), Position (V2 x y), Paths pathscape allgoals, MapGrid grid size) -> if null cpath && isJust mGoals then do
    let startPoint = head . dropWhile (maybe False (not . walkable) . tileOfCoord grid size ) $ (V2 x y):(concat $ iterate (\[V2 x1 y1,V2 x2 y2,V2 x3 y3,V2 x4 y4] -> [V2 (x1+4) y1,V2 (x2-4) y2,V2 x3 (y3+4),V2 x4 (y4-4)]) (take 4 $ repeat (V2 x y)))
        path  = findPathToClosest pathscape (fromJust mGoals) startPoint
    return $ maybe p (PathFinder mGoals) path
    else return p


checkGoal :: (HasMany w [PathFinder, Position]) => System w ()
checkGoal = cmap $ \(p@(PathFinder goals pathNodes), Position pos) ->
    if null pathNodes then p
    else let node = head pathNodes in if sqDistance node pos < 16 then PathFinder goals (tail pathNodes)  else p


findPathToClosest graph goals = aStar
    (fromMaybe HS.empty . (`M.lookup` graph) . tileCentre 2 )
    (\a b -> sqrt (sqDistance a b))
    (\p -> sqrt (minimum $ map (sqDistance p) goals))
    (\loc -> any ((<=128) . sqDistance loc) goals)

sqDistance :: Num a => V2 a -> V2 a -> a
sqDistance (V2 x1 y1) (V2 x2 y2) = (x1-x2)^2 + (y1-y2)^2


generateGraph :: (V2 Float -> Maybe Tile) -> [V2 Float] -> PathfindGraph
generateGraph toTile = foldr (\n -> M.insert n (HS.fromList (findAdjacent toTile n))) M.empty


findAdjacent :: (V2 Float -> Maybe Tile) -> V2 Float -> [V2 Float]
findAdjacent toTile target@(V2 x y) = filter (moveable toTile target) [V2 (x-64) y,V2 (x+64) y,V2 x (y+64),V2 x (y-64),V2 (x-64) (y-64),V2 (x+64) (y+64),V2 (x+64) (y-64), V2 (x-64) (y+64)]

moveable :: (V2 Float -> Maybe Tile) -> V2 Float -> V2 Float -> Bool
moveable toTile p1 p2 =  let
    isWalkable point = maybe False walkable $ toTile point
    in isWalkable p1 && isWalkable p2

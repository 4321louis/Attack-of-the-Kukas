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

type PathfindGraph = M.Map (Float,Float) (HS.HashSet (Float,Float))

data Paths = Paths PathfindGraph [(Float,Float)] deriving (Show)
instance Semigroup Paths where (Paths a g1) <> (Paths b g2) = Paths (M.union a b) (g1 ++ g2)
instance Monoid Paths where mempty = Paths M.empty []
instance Component Paths where type Storage Paths = Global Paths

data PathFinder = PathFinder (Maybe [(Float,Float)]) [(Float,Float)] deriving (Show)
instance Component PathFinder where type Storage PathFinder = Map PathFinder

doPathFinding :: (HasMany w [PathFinder, Paths, Position, Velocity]) => System w ()
doPathFinding = do
    acquireNewPaths
    checkGoal
    moveOnPath


acquireNewPaths :: (HasMany w [PathFinder, Paths, Position]) => System w ()
acquireNewPaths = cmapM $ \(p@(PathFinder mGoals cpath), Position (V2 x y)) -> if null cpath && isJust mGoals then do
    Paths pathscape allgoals <- get global
    let path  = findPathToClosest pathscape allgoals (x,y)
    return $ maybe p (PathFinder (Just allgoals)) path
    else return p


checkGoal :: (HasMany w [PathFinder, Position]) => System w ()
checkGoal = cmap $ \(p@(PathFinder goals pathNodes), Position (V2 px py)) -> 
    if null pathNodes then p 
    else let node = head pathNodes in if sqDistance node (px,py) < 16 then PathFinder goals (tail pathNodes)  else p

moveOnPath :: (HasMany w [PathFinder, Position, Velocity]) => System w ()
moveOnPath = cmap $ \(PathFinder _ pathNodes, Position pos, Velocity _) ->
    if null pathNodes
    then Velocity (V2 0 0)
    else let (nx,ny) = head pathNodes in Velocity ((L.^* 20) . L.normalize $ V2 nx ny - pos)


findPathToClosest graph goals = aStar
    (\(x,y)-> fromMaybe HS.empty . (`M.lookup` graph) $ (32 + fromIntegral (floorMultiple x 64),32 + fromIntegral (floorMultiple y 64)))
    (\a b -> sqrt (sqDistance a b))
    (\p -> sqrt (minimum $ map (sqDistance p) goals))
    (\loc -> any ((<=2048) . sqDistance loc) goals)

sqDistance :: Num a => (a, a) -> (a, a) -> a
sqDistance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2


generateGraph :: ((Float,Float) -> Maybe Tile) -> [(Float,Float)] -> PathfindGraph
generateGraph toTile = foldr (\n -> M.insert n (HS.fromList (findAdjacent toTile n))) M.empty


findAdjacent :: ((Float,Float) -> Maybe Tile) -> (Float,Float) -> [(Float,Float)]
findAdjacent toTile target@(x,y) = filter (moveable toTile target) [(x - 64, y),(x + 64, y),(x, y + 64),(x, y - 64),(x - 64, y - 64),(x + 64, y + 64),(x + 64, y - 64),(x - 64, y + 64)]

moveable :: ((Float,Float) -> Maybe Tile) -> (Float, Float) -> (Float, Float) -> Bool
moveable toTile p1 p2 =  let
    isWalkable (x,y) = maybe False walkable $ toTile (x,y)
    in isWalkable p1 && isWalkable p2

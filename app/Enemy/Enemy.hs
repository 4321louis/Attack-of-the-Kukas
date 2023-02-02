-- Enemy Behaviour

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Enemy.Enemy where

import Apecs
import Apecs.Extension
import Structure.Structure
import Enemy.Pathfinding
import qualified Linear as L
import Worlds
import Misc
import Linear (V2(..))
--dmg, speed
data Enemy = Enemy Float Float deriving (Show)
instance Component Enemy where type Storage Enemy = Map Enemy

doEnemy :: (HasMany w [PathFinder, Position, Velocity, Enemy, Paths, Structure, Time, Hp]) => Float -> System w ()
doEnemy dT = do
    moveOnPath
    attackOrNewPath dT


moveOnPath :: (HasMany w [PathFinder, Position, Velocity, Enemy]) => System w ()
moveOnPath = cmap $ \(p@(PathFinder _ pathNodes), Position pos, Velocity _, Enemy _ speed) ->
    if null pathNodes
    then (PathFinder Nothing [],Velocity (V2 0 0))
    else (p,Velocity ((L.^* speed) . L.normalize $ (head pathNodes) - pos))


attackOrNewPath :: (HasMany w [PathFinder, Position, Velocity, Enemy, Paths, Structure, Time, Hp]) => Float -> System w ()
attackOrNewPath dT = cmapM $ \(p@(PathFinder _oldGoals pathNodes), Position epos, Velocity _, Enemy dmg _, Paths _ goals) -> 
    let trueGoals = if null goals then Nothing else Just goals in
    if null pathNodes
    then do
        (cdist, closest) <- cfold (\min@(minDist,_) (Structure _, Position spos, ety) ->
            let nDist = L.norm (spos - epos)
            in if nDist < minDist then (nDist,ety) else min) (10000,0)
        if cdist < 112 then triggerEvery dT 1 0 (modify closest (\(Structure _, hp) -> dealDamage hp dmg)) >> return p else return (PathFinder trueGoals [])
    else return (PathFinder trueGoals pathNodes)

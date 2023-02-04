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
import Apecs.Gloss (Camera(..))
import Structure.Structure
import Enemy.Pathfinding
import qualified Linear as L
import Worlds
import Misc
import Linear (V2(..))
import Drawing.Sprites
import Control.Monad
import System.Random (randomRIO)
import Plant.Seed
import Audio
--dmg, speed
data Enemy = Enemy Float Float deriving (Show)
instance Component Enemy where type Storage Enemy = Map Enemy

newtype DropHandler = DropHandler Float deriving (Show, Num)
instance Semigroup DropHandler where (<>) = (+)
instance Monoid DropHandler where mempty = 50
instance Component DropHandler where type Storage DropHandler = Global DropHandler

type AllEnemyComps = (Position, Enemy, Velocity, PathFinder, Sprite, Hp)

destroyDeadEnemies :: (HasMany w [PathFinder, Position, Velocity, Enemy, Sprite, Hp, DropHandler, Seed, EntityCounter, DropHandler, Camera]) => System w ()
destroyDeadEnemies =
    cmapM $ \(Enemy _ _, Hp hp _ _ , ety, Position pos, DropHandler chance) ->
        if hp <= 0
        then do
            destroy ety (Proxy @AllEnemyComps )
            playIOSoundEffectAt pos kukasDeath
            roll <- randomRIO (0,100)
            if roll < chance
            then do
                seed <- liftIO $ randomRIO (0,3)
                xoff <- liftIO $ randomRIO (-32,32)
                yoff <- liftIO $ randomRIO (-32,32)
                _ <- newEntity ([GreenSeed,RedSeed,BlueSeed,Spore] !! seed, Position (V2 xoff yoff + pos), Sprite $ [greenSeed,redSeed,blueSeed,spore] !! seed )
                return (DropHandler 0)
            else return (DropHandler (chance+4))
        else return (DropHandler chance)


doEnemy :: (HasMany w [PathFinder, Position, Velocity, AnimatedSprite, Enemy, Paths, Structure, Time, Hp, Camera]) => Float -> System w ()
doEnemy dT = do
    moveOnPath
    attackOrNewPath dT
    chooseSpirte

chooseSpirte :: HasMany w [AnimatedSprite, Velocity] => System w ()
chooseSpirte = cmap $ \(AnimatedSprite r ls,Velocity v@(V2 x y)) -> 
    if v == V2 0 0 then droneKukasWalkRight
    else if x>0 then droneKukasWalkRight else droneKukasWalkLeft 
moveOnPath :: (HasMany w [PathFinder, Position, Velocity, Enemy]) => System w ()
moveOnPath = cmap $ \(p@(PathFinder _ pathNodes), Position pos, Velocity _, Enemy _ speed) ->
    if null pathNodes
    then (PathFinder Nothing [],Velocity (V2 0 0))
    else (p,Velocity ((L.^* speed) . L.normalize $ head pathNodes - pos))


attackOrNewPath :: (HasMany w [PathFinder, Position, Velocity, Enemy, Paths, Structure, Time, Hp, Camera]) => Float -> System w ()
attackOrNewPath dT = cmapM $ \(p@(PathFinder _oldGoals pathNodes), Position epos, Velocity _, Enemy dmg _, Paths _ goals) ->
    let trueGoals = if null goals then Nothing else Just goals in
    if null pathNodes
    then do
        (cdist, closest) <- cfold (\min@(minDist,_) (Structure _, Position spos, ety) ->
            let nDist = L.norm (spos - epos)
            in if nDist < minDist then (nDist,ety) else min) (10000,0)
        if cdist < 112 then triggerEvery dT 1 0 (do
            modify closest (\(Structure _, hp) -> dealDamage hp dmg)
            playIOSoundEffectAt epos kukasAttack) >> return p
        else return (PathFinder trueGoals [])
    else return (PathFinder trueGoals pathNodes)

-- A file for very General concepts that are global or apply to a large set of different entities

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Worlds where

import Apecs
import Linear (V2(..))
import qualified Linear as L
import Apecs.Extension (HasMany)

newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity

type Kinetic = (Position, Velocity)

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

-- cur, max, shield
data Hp = Hp Float Float Float deriving (Show)
instance Component Hp where type Storage Hp = Map Hp

newtype MovementPattern = MovementPattern (Float -> Position)
instance Component MovementPattern where type Storage MovementPattern = Map MovementPattern

orbitalPattern :: V2 Float -> Float -> Float -> Float -> Float -> Position
orbitalPattern center radius period toffset t = Position $ center + radius L.*^ V2 (cos (2*pi*t'/period)) (sin (2*pi*t'/period)) where t' = t -toffset

starPattern :: V2 Float -> Float -> Float -> Float -> Float -> Position
starPattern center radius period toffset t = Position $ center + radius L.*^ V2 (2/7*cos (6*pi*t'/period) + 5/7*sin (4*pi*t'/period)) (2/7*sin (6*pi*t'/period) + 5/7*cos (4*pi*t'/period)) where t' = t -toffset

incrTime :: (Has w IO Time) => Float -> System w ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

stepPosition ::  (HasMany w [Position, Velocity]) => Float -> System w ()
stepPosition dT = do 
    cmap $ \(Position p, Velocity v) -> Position (p + v L.^* dT)

dealDamage :: Hp -> Float -> Hp
dealDamage (Hp hp m shield) dmg =
    let unShielded = max 0 $ dmg - shield
    in Hp (hp - unShielded) m (max 0 (shield - dmg))

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

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

-- cur, max, shield
data Hp = Hp Float Float Float deriving (Show)
instance Component Hp where type Storage Hp = Map Hp

incrTime :: (Has w IO Time) => Float -> System w ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

stepPosition ::  (HasMany w [Position, Velocity]) => Float -> System w ()
stepPosition dT = do 
    cmap $ \(Position p, Velocity v) -> Position (p + v L.^* dT)

dealDamage :: Hp -> Float -> Hp
dealDamage (Hp hp m shield) dmg =
    let unShielded = max 0 $ dmg - shield
    in Hp (hp - unShielded) m (max 0 (shield - dmg))

healHp :: Hp -> Float -> Hp
healHp (Hp hp m shield) heal
    | hp + heal > m = Hp m m shield
    | otherwise = Hp (hp + heal) m shield

shieldHp :: Hp -> Float -> Hp
shieldHp (Hp hp m shield) shielding = Hp hp m shielding
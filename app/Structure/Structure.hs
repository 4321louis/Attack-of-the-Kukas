-- Things that can be attacked by enemies

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Structure.Structure where

import Apecs
import Apecs.Extension
import Apecs.Gloss
import Enemy.Pathfinding
import Linear (V2(..))
import Drawing.Sprites
import Worlds

-- HP, attack positions
newtype Structure = Structure [V2 Float] deriving (Show)
instance Component Structure where type Storage Structure = Map Structure

data Base = Base deriving (Show)
instance Component Base where type Storage Base = Unique Base

updateGoals :: (HasMany w [Paths, Structure]) => System w ()
updateGoals = do
    modify global $ \(Paths graph _) -> Paths graph []
    cmapM_ $ \(Structure points) -> do modify global $ \(Paths graph goals) -> Paths graph (points ++ goals)



dealDamage :: (HasMany w [Position, Velocity, Particle, Sprite, EntityCounter]) => Position -> Hp -> Float -> System w Hp
dealDamage posP (Hp hp m shield) dmg = do
    newEntity (Particle 0.4, posP, Velocity (V2 0 48), Sprite (translate (-16) 0 $ scale 0.08 0.08 $ color red .  Text $ show (round dmg::Integer)))
    let unShielded = max 0 $ dmg - shield
    return $ Hp (hp - unShielded) m (max 0 (shield - dmg))

healHp :: (HasMany w [Position, Velocity, Particle, Sprite, EntityCounter]) => Position -> Hp -> Float -> System w Hp
healHp posP (Hp hp m shield) heal = do 
    newEntity (Particle 0.4, posP, Velocity (V2 0 48), Sprite (translate (-16) 0 $ scale 0.08 0.08 . color chartreuse .  Text $ show (round heal::Integer)))
    return $ (if hp + heal > m then Hp m else Hp (hp + heal) ) m shield

shieldHp :: (HasMany w [Position, Velocity, Particle, Sprite, EntityCounter]) => Position -> Hp -> Float -> System w  Hp
shieldHp posP (Hp hp m shield) shielding = do
    newEntity (Particle 0.4, posP, Velocity (V2 0 48), Sprite (translate (-16) 0 $ scale 0.08 0.08 $ color cyan .  Text $ show (round shielding::Integer)))
    return $ Hp hp m shielding

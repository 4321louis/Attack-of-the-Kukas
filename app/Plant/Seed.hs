
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

module Plant.Seed where

import Apecs
import Apecs.Gloss
import Drawing.Sprites

data Seed = GreenSeed | RedSeed | BlueSeed | Spore deriving (Show, Eq)
instance Component Seed where type Storage Seed = Map Seed

-- Inventory (inventory, craft)
-- inventory order: [Green, Red, Blue, Spore]
data Inventory = Inventory [Int] [Seed] deriving (Show)
instance Semigroup Inventory where Inventory a1 a2 <> Inventory b1 b2 = Inventory (a1 ++ b1) (a2 ++ b2)
instance Monoid Inventory where mempty = Inventory [] []
instance Component Inventory where type Storage Inventory = Global Inventory

getSeedSprite :: Seed -> Picture
getSeedSprite GreenSeed = greenSeed
getSeedSprite RedSeed = redSeed
getSeedSprite BlueSeed = blueSeed
getSeedSprite Spore = spore
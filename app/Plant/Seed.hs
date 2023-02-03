
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

data Seed = GreenSeed | RedSeed | BlueSeed | Spore deriving (Show)
instance Component Seed where type Storage Seed = Map Seed

newtype Craft = Craft [Seed] deriving (Show)
instance Semigroup Craft where Craft a <> Craft b = Craft (a ++ b)
instance Monoid Craft where mempty = Craft []
instance Component Craft where type Storage Craft = Global Craft

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
    
import Drawing.Sprites
import Apecs
import Apecs.Gloss
import Apecs.Extension
import Control.Monad
import Misc
import Graphics.Gloss.Game (jpg,png)
import System.Random
import qualified Linear as L

import Enemy.Enemy
import Worlds

data Seed = Seed deriving (Show)
instance Component Seed where type Storage Seed = Map Seed

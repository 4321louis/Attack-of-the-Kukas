-- Misc general functions

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

module Misc where
    
import Worlds (Position(..),Time(..))

import Apecs
import Apecs.Gloss
import Control.Monad

import Linear (V2(..))

translatePos :: Position -> Picture -> Picture
translatePos (Position (V2 x y)) = translate x y
translateV2 :: V2 Float -> Picture -> Picture
translateV2 (V2 x y) = translate x y


triggerEvery :: (Has w IO Time) => Float -> Float -> Float -> System w a -> System w ()
triggerEvery dT period phase sys = do
    Time t <- get global
    let t' = t + phase
        trigger = floor (t' / period) /= floor ((t' + dT) / period)
    when trigger $ void sys

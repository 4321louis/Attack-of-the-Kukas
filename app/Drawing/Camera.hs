--Performing actions on or around the camera

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

module Drawing.Camera where

import Apecs
import Apecs.Gloss
import Apecs.Extension

import Player (Player(..))
import Misc
import Worlds

camOnPlayer :: (HasMany w [Player, Position, Camera]) => System w ()
camOnPlayer = cmap $ \(Player, Position pos , Camera _ cscale) ->
    Camera pos cscale

rescaleCam :: (Has w IO Camera) => Float -> System w ()
rescaleCam dt = modify global $ \(Camera pos cScale) -> Camera pos (min 1 (cScale + dt*0.9))

pictureOnHud :: Camera -> Picture -> Picture
pictureOnHud (Camera cameraOffset cameraScale) = translateV2 cameraOffset . scale (1/cameraScale) (1/cameraScale)
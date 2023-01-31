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
import Apecs.Gloss

-- HP, attack positions?
data Structure = Structure Float [(Float,Float)] deriving (Show)
instance Component Structure where type Storage Structure = Map Structure

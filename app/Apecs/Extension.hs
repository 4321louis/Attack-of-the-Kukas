
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Apecs.Extension where
import Data.Kind
import Apecs

type family HasMany w xs :: Constraint where
  HasMany w '[] = ()
  HasMany w (c ':cs) = (Has w IO c, HasMany w cs)

modifyM :: (Get w m cx, Set w m cy) => Entity -> (cx -> SystemT w m cy) -> SystemT w m ()
modifyM ety func = do
  comps <- get ety
  result <- func comps
  set ety result
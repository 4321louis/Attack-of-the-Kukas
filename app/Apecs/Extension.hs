
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Apecs.Extension where
import Data.Kind
import Apecs

type family HasMany w xs :: Constraint where
  HasMany w '[] = ()
  HasMany w (c ':cs) = (Has w IO c, HasMany w cs)
{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Hand-rolled geometric calculi:
-- Umbral Calculus, Grassmann Ausdehnungslehre (1844),
-- and Schubert Enumerative Calculus.

module MiniGHC.Geometry
  ( module MiniGHC.Geometry.Umbral
  , module MiniGHC.Geometry.Grassmann
  , module MiniGHC.Geometry.Schubert
  ) where

import MiniGHC.Geometry.Umbral
import MiniGHC.Geometry.Grassmann
import MiniGHC.Geometry.Schubert

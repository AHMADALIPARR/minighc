{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

-- | Classic lambda calculi.
--
-- The individual calculi live in separate modules to avoid name clashes.
-- Import them directly:
--
--   * "MiniGHC.Calculi.STLC"
--   * "MiniGHC.Calculi.SystemF"
--   * "MiniGHC.Calculi.HM"

module MiniGHC.Calculi (inferHM, inferSTLC, inferSystemF) where
import qualified MiniGHC.Calculi.HM as HM
import qualified MiniGHC.Calculi.STLC as STLC
import qualified MiniGHC.Calculi.SystemF as SF

inferHM :: HM.Tm -> Maybe HM.Ty
inferHM = HM.infer
inferSTLC :: STLC.Ctx -> STLC.Tm -> Maybe STLC.Ty
inferSTLC = STLC.synth
inferSystemF :: SF.Ctx -> SF.Tm -> Maybe SF.Ty
inferSystemF = SF.synth

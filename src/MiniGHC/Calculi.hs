{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}

{-# LANGUAGE GADTs, LambdaCase #-}

-- | Classic lambda calculi that form the theoretical basis of Haskell
-- and of the MiniGHC intermediate languages.
--
--   * Simply-typed lambda calculus (STLC)
--   * System F (polymorphic lambda calculus)
--   * A small Hindley–Milner fragment
--
-- Each calculus is given its syntax, a type checker, and a call-by-value
-- evaluator. These are independent of the main MiniGHC pipeline but share
-- the same design discipline.

module MiniGHC.Calculi
  ( -- * Simply-typed lambda calculus
    STLC.Ty(..), STLC.Tm(..), STLC.synth, STLC.eval
    -- * System F
  , SF.Ty(..), SF.Tm(..), SF.synth, SF.eval
    -- * Hindley–Milner fragment
  , HM.Ty(..), HM.Tm(..), HM.infer
  ) where

import qualified MiniGHC.Calculi.STLC as STLC
import qualified MiniGHC.Calculi.SystemF as SF
import qualified MiniGHC.Calculi.HM as HM

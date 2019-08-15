{-# LANGUAGE TemplateHaskell #-}

-- | `STSExtra` typeclass and associated instances.
module STSExtra where

import           Control.State.Transition (PredicateFailure, STS)
import           Cardano.Ledger.Spec.STS.UTXO ()
import           Cardano.Ledger.Spec.STS.UTXOW ()
import           Cardano.Ledger.Spec.STS.UTXOWS ()
import           Cardano.Spec.Chain.STS.Rule.BBody ()
import           Cardano.Spec.Chain.STS.Rule.Bupi ()
import           Cardano.Spec.Chain.STS.Rule.Chain ()
import           Cardano.Spec.Chain.STS.Rule.Epoch ()
import           Cardano.Spec.Chain.STS.Rule.Pbft ()
import           Cardano.Spec.Chain.STS.Rule.SigCnt ()
import           Ledger.Update ()
import           Ledger.Delegation ()

import STSExtra.TH


-- | Predicate to compare equality of `PredicateFailure`s. Used for
-- breeding goblins to target a specific `PredicateFailure`.
class STS s => STSExtra s where
  -- | This predicate allows us to compare PredicateFailures "modulo fields"
  -- - only checking equality of PredicateFailure constructors, recursively,
  -- "down" the datatype.
  eqPF :: PredicateFailure s -> PredicateFailure s -> Bool

  -- | This function renders a PredicateFailure solely in terms of the
  -- PredicateFailure constructors contained within it. It ignores the
  -- extraneous fields (e.g. of type `Int`).
  renderPF :: PredicateFailure s -> String

deriveSTSExtraInstancesForAllPredicateFailures

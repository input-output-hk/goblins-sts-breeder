{-# LANGUAGE TemplateHaskell #-}

module STSExtra.TH where

import           Control.Monad (replicateM)
import           Language.Haskell.TH
import           TH.ReifySimple


-- | Derive STSExtra instances for all STS instances.
--
-- An example resultant instance.
-- @
--   instance STSExtra Cardano.Ledger.Spec.STS.UTXOW.UTXOW
--       where eqPF (Cardano.Ledger.Spec.STS.UTXOW.UtxoFailure arg_0) (Cardano.Ledger.Spec.STS.UTXOW.UtxoFailure arg_1) = arg_0 `eqPF` arg_1
--             eqPF (Cardano.Ledger.Spec.STS.UTXOW.InsufficientWitnesses) (Cardano.Ledger.Spec.STS.UTXOW.InsufficientWitnesses) = True
--             eqPF _ _ = False
--
--             renderPF (Cardano.Ledger.Spec.STS.UTXOW.UtxoFailure arg_2) = "UtxoFailure" GHC.Base.<> ("_" GHC.Base.<> renderPF arg_2)
--             renderPF (Cardano.Ledger.Spec.STS.UTXOW.InsufficientWitnesses) = "InsufficientWitnesses"
-- @
deriveSTSExtraInstancesForAllPredicateFailures :: Q [Dec]
deriveSTSExtraInstancesForAllPredicateFailures = do
  (DataFamily _name _tvs insts) <- reifyDataFamily (mkName "PredicateFailure")
  mapM mkSTSExtraInstances insts
 where
  mkSTSExtraInstances :: DataInst -> Q Dec
  mkSTSExtraInstances di = do
    let headTy = case diParams di of
                   [] -> error "no head type in data instance"
                   [ht] -> ht
                   _ -> error "too many head types in data instance"
    let decTy = AppT (ConT (mkName "STSExtra")) headTy
    InstanceD Nothing [] decTy
      <$> sequence (map ($ (diCons di)) [mkEqPFDefn, mkRenderPFDefn])

  mkEqPFDefn :: [DataCon] -> Q Dec
  mkEqPFDefn dcs = do
    -- If there is more than 1 DataCon, we won't cover all n^2 cases via our
    -- n^1 enumeration of pairs, so we add a fallthrough case which returns
    -- `False`. If there are 0 DataCons, then we add a fallthrough case
    -- so we don't have an empty instance declaration.
    let fallThrough = case length dcs `compare` 1 of
                        EQ -> []
                        _  -> [mkFallThroughFalseClause]
    FunD (mkName "eqPF") <$> (sequence (map mkClauses dcs <> fallThrough))
   where
    mkClauses :: DataCon -> Q Clause
    mkClauses dc = ifEmbeddedPFThenElse (map snd (dcFields dc))

      -- `Embed`ed PFs - recur on argument.
      (do
       pairs <- replicateM 2 $ do
         arg <- newName "arg"
         pure ( ConP (dcName dc) [VarP arg]
              , arg
              )
       let pats = map fst pairs
           (lhs, rhs) = case map snd pairs of
                          [a,b] -> (a,b)
                          _     -> error "impossible"
       let body = NormalB (UInfixE (VarE lhs) (VarE (mkName "eqPF")) (VarE rhs))
       pure (Clause pats body []))

      -- No `Embed`ed PFs - match constructors.
      (do
       let wildCards = map (const WildP) (dcFields dc)
       let pat = ConP (dcName dc) wildCards
       let pats = [pat, pat]
       let body = NormalB (ConE (mkName "True"))
       pure (Clause pats body []))


  mkRenderPFDefn :: [DataCon] -> Q Dec
  mkRenderPFDefn dcs = do
    clauses <- if length dcs == 0
                  then (:[]) <$> fallThroughDie
                  else sequence (map mkClauses dcs)
    pure (FunD funcName clauses)
   where
    funcName = mkName "renderPF"

    mkClauses :: DataCon -> Q Clause
    mkClauses dc = do
      let constrE = LitE (StringL (nameBase (dcName dc)))

      (pat, bodyE) <- ifEmbeddedPFThenElse (map snd (dcFields dc))

        -- Single `Embed`ed PF - recur on argument.
        (do
         arg <- newName "arg"
         let pat = ConP (dcName dc) [VarP arg]
             recExp = AppE (VarE funcName) (VarE arg)
         bodyE <- [| $(pure constrE) <> "_" <> $(pure recExp) |]
         pure (pat, bodyE))

        -- No `Embed`ed PFs - finish recursion.
        (do
         let wildCards = map (const WildP) (dcFields dc)
             pat = ConP (dcName dc) wildCards
         pure (pat, constrE))

      pure (Clause [pat] (NormalB bodyE) [])

    fallThroughDie :: Q Clause
    fallThroughDie = do
      bodyE <- [| error "impossible: can't be triggered" |]
      pure (Clause [] (NormalB bodyE) [])


  mkFallThroughFalseClause :: Q Clause
  mkFallThroughFalseClause =
    pure (Clause [WildP, WildP] (NormalB (ConE (mkName "False"))) [])


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | If the list of types contains a single `AppT` of a `PredicateFailure`, then return the first value. If there are multiple such types, error. Otherwise, return the second value.
ifEmbeddedPFThenElse :: [Type] -> a -> a -> a
ifEmbeddedPFThenElse tys x y
  -- We have an `Embed`ded `PredicateFailure`. Match both arguments to get
  -- out the embedded PFs, and call `eqPF` on them.
  | [fieldTy] <- tys
  , checkAppPF fieldTy
  = x

  -- We have embedded PFs but also other fields. This shouldn't happen.
  | or (map checkAppPF tys)
  = error "invalid `Embed` wrapper PredicateFailure: expected one field"

  -- We have non-PF fields. Insert clauses which ignore the fields and
  -- only compare constructors for equality.
  | otherwise
  = y

-- | Returns True if the type is an `AppT` applied to a `PredicateFailure`.
-- This indicates that our type is a `PredicateFailure` of some STS.
checkAppPF :: Type -> Bool
checkAppPF (AppT (ConT x) _)
  | nameBase x == "PredicateFailure" = True
  | otherwise = False
checkAppPF _ = False

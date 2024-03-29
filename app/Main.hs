{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent.Async (async, wait)
import           Control.Monad (forM, forM_, replicateM_)
import           Data.List (partition)
import           Data.List.Extra (chunksOf)
import           Data.Time.Clock (diffUTCTime, getCurrentTime)
import           Data.TreeDiff.Class
import           Data.TreeDiff.Expr
import qualified Options.Applicative as O
import           System.FilePath.Posix ((</>), (<.>))
import           System.Directory (createDirectoryIfMissing)
import           System.IO (writeFile)
-- import           Text.PrettyPrint (render)

import           Cardano.Ledger.Spec.STS.UTXO (PredicateFailure(..))
import           Cardano.Ledger.Spec.STS.UTXOW (UTXOW, PredicateFailure(..))
import           Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.BBody (BBODY, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.Bupi (BUPI, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.Chain (CHAIN, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.Epoch (EPOCH, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.Pbft as Pbft (PBFT, PredicateFailure(..))
import           Cardano.Spec.Chain.STS.Rule.SigCnt as Pbft (PredicateFailure(..))
import           Control.State.Transition (Signal(..))
import           Control.State.Transition.Generator (HasTrace(..))
import           Ledger.Delegation
  (DELEG, PredicateFailure(..), EpochDiff(..))
import           Ledger.Update as Update
  ( ApName(..)
  , ApVer(..)
  , UpId(..)
  , ADDVOTE
  , UPIEND
  , UPIREG
  , UPIVOTES
  , UPPVV
  , UPSVV
  , UPV
  , PredicateFailure(..)
  , ProtVer(..)
  )
import           Ledger.Core
  ( Epoch(..)
  , Hash(..)
  , Owner(..)
  , Sig(..)
  , Slot(..)
  , VKey(..)
  , VKeyGenesis(..)
  )
import           Test.Goblin

import BreedingPit
  ( aboveFitnessThreshold
  , breedStsGoblins
  , fitnessThreshold
  , genEnvStateSig
  , Gen
  , Population
  , BreederConfig(..)
  )
import Parse
import STSExtra


allRunsDir :: FilePath
allRunsDir = "runs"

main :: IO ()
main = do
  (runCount, breederConfig) <- O.execParser opts
  putStrLn ("Preparing for " <> show runCount <> " runs.")
  createDirectoryIfMissing True allRunsDir
  replicateM_ runCount (trainGoblins breederConfig)

trainGoblins :: BreederConfig -> IO ()
trainGoblins breederConfig = do
  -- Set up directories.
  let runIxFile = allRunsDir </> "count" <.> "txt"
  runIx <- parseRunIndexFromFile runIxFile
  let runDir    = allRunsDir </> show runIx
      genomeDir = runDir </> "goblin_genomes"
      passDir   = genomeDir </> "pass"
      failDir   = genomeDir </> "fail"
  mapM_ (createDirectoryIfMissing True) [passDir, failDir]

  let teeIt = tee (runDir </> "log" <.> "txt")
  teeIt ("Run index: " <> show runIx)

  -- Number of parallel jobs to run
  let numParallelJobs = 1
  forM_ (chunksOf numParallelJobs (breeders breederConfig)) $ \structs -> do
    startTime <- getCurrentTime
    actions <- forM structs $ \(PopStruct name action wrappedGenSigs) -> async $ do
      pop <- action
      pure (pop, name, wrappedGenSigs)
    results <- traverse wait actions
    endTime <- getCurrentTime
    teeIt ("Training time was: " <> show (endTime `diffUTCTime` startTime))

    let (good, bad) = partition (\(pop,_,_) -> aboveFitnessThreshold (snd (head pop))) results
    teeIt ("Total: " <> show (length results) <> ". Good: " <> show (length good) <> ". Bad: " <> show (length bad))
    forM_ good $ \(pop, name, _) -> do
      teeIt ("PASS: " <> name <> " generated goblins. Best adjusted score: " <> show (snd (head pop) - fitnessThreshold) <> ".")
      writePopulationToFile (passDir </> name) (take 10 pop)
    forM_ bad $ \(pop, name, _) -> do
      teeIt ("FAIL: " <> name <> " didn't generate good goblins.")
      writePopulationToFile (failDir </> name) (take 10 pop)
    teeIt ""

tee :: FilePath -> String -> IO ()
tee fp msg = do
  putStrLn msg
  appendFile fp (msg++"\n")

explainTheGoblin :: forall sts
                  . (Goblin Bool (Signal sts), ToExpr (Signal sts))
                 => [Gen (Signal sts)]
                 -> GoblinData Bool
                 -> [Edit EditExpr]
explainTheGoblin genSigs goblin =
  map (\g -> maybe (error "explainTheGoblin: got Nothing") (\(_,_,diff,_) -> diff) (explainGoblinGen Nothing Nothing g goblin))
      genSigs


breedType :: forall sts
           . ( Goblin Bool (Signal sts), HasTrace sts, STSExtra sts
             , SeedGoblin (Environment sts), SeedGoblin (State sts))
          => BreederConfig
          -> (Gen (Signal sts) -> WrappedGenSig)
          -> PredicateFailure sts
          -> ( IO (Population Bool)
             , WrappedGenSig
             )
breedType breederConfig wrapper predicateFailure =
  ( do { vs <- breedStsGoblins @sts breederConfig predicateFailure
       ; writeFile "/dev/null" (show (snd (head vs))); pure vs }
  , wrapper genSig
  )
 where
  genSig = snd <$> (genEnvStateSig @sts)

{-

-- this is massive - the toplevel STS, more or less
HasTrace CHAIN         ./byron/chain/executable-spec/src/Cardano/Spec/Chain/STS/Rule/Chain.hs  168

-- this is an example, don't need to check
-- HasTrace SUM           ./byron/semantics/executable-spec/test/Control/State/Transition/Examples/Sum.hs 38

  -- this is in a test package, which will need to be factored out
  -- HasTrace DBLOCK        ./byron/ledger/executable-spec/test/Ledger/Delegation/Properties.hs     285
  , map (breedType WrapDBLOCK)
        ( NotIncreasingBlockSlot
        : (map DPF delegPFs))

  -- ^ same for this
  -- HasTrace UBLOCK        ./byron/ledger/executable-spec/test/Ledger/Update/Properties.hs 358
-}

data PopStruct = PopStruct
  { popStructName :: String
  , popStructPop  :: IO (Population Bool)
  , popStructSigs :: WrappedGenSig
  }


breeders :: BreederConfig -> [PopStruct]
breeders breederConfig = concat $ take 4 [

  -- HasTrace DELEG         byron/ledger/executable-spec/src/Ledger/Delegation.hs
    map (\pf -> uncurry (PopStruct ("DELEG_" <> renderPF pf)) (breedType breederConfig WrapDELEG pf))
        delegPFs

  -- HasTrace UTXOW         byron/ledger/executable-spec/src/Cardano/Ledger/Spec/STS/UTXOW.hs
  , map (\pf -> uncurry (PopStruct ("UTXOW_" <> renderPF pf)) (breedType breederConfig WrapUTXOW pf))
        utxowPFs

  -- HasTrace UPIREG        byron/ledger/executable-spec/src/Ledger/Update.hs
  , map (\pf -> uncurry (PopStruct ("UPIREG_" <> renderPF pf)) (breedType breederConfig WrapUPIREG pf))
        upiregPFs

  -- HasTrace UPIVOTES      byron/ledger/executable-spec/src/Ledger/Update.hs
  , map (\pf -> uncurry (PopStruct ("UPIVOTES_" <> renderPF pf)) (breedType breederConfig WrapUPIVOTES pf))
        upivotesPFs

  -- HasTrace UTXOWS        byron/ledger/executable-spec/src/Cardano/Ledger/Spec/STS/UTXOWS.hs
  , map (\pf -> uncurry (PopStruct ("UTXOWS_" <> renderPF pf)) (breedType breederConfig WrapUTXOWS pf))
        (map UtxowFailure utxowPFs)

  -- HasTrace CHAIN         byron/chain/executable-spec/src/Cardano/Spec/Chain/STS/Rule/Chain.hs
  , map (\pf -> uncurry (PopStruct ("CHAIN_" <> renderPF pf)) (breedType breederConfig WrapCHAIN pf))
        chainPFs

  ]
 where
  delegPFs :: [PredicateFailure DELEG]
  delegPFs = (concat [ (map (SDelegSFailure . SDelegFailure)
                            [ IsNotGenesisKey
                            , EpochInThePast (EpochDiff (Epoch 1) (Epoch 0))
                            , EpochPastNextEpoch (EpochDiff (Epoch 0) (Epoch 1))
                            , HasAlreadyDelegated
                            , IsAlreadyScheduled
                            , Ledger.Delegation.DoesNotVerify
                            ])
                     {-
                     -- The disjuction of the preconditions here is `True`, so
                     -- none of these `PredicateFailure`s are throwable. Thus
                     -- we don't bother training goblins to target them.
                     , (map (ADelegSFailure . ADelegFailure)
                            [ S_BeforeExistingDelegation
                            , S_NoLastDelegation
                            , S_AfterExistingDelegation
                            , S_AlreadyADelegateOf (VKey (Owner 1)) (VKeyGenesis (VKey (Owner 2)))
                            ])
                     -}
                     ])

  utxowPFs :: [PredicateFailure UTXOW]
  utxowPFs = ([ InsufficientWitnesses
              ] ++ (map UtxoFailure
                        [ EmptyTxInputs
                        , EmptyTxOutputs
                        , FeeTooLow
                        , IncreasedTotalBalance
                        , InputsNotInUTxO
                        , NonPositiveOutputs
                        ]))

  utxowsPFs :: [PredicateFailure UTXOWS]
  utxowsPFs = map UtxowFailure utxowPFs

  upiregPFs :: [PredicateFailure UPIREG]
  upiregPFs = map UPREGFailure ([ NotGenesisDelegate
                                , Update.DoesNotVerify
                                ] ++ (map UPVFailure upvPFs))

  upvPFs :: [PredicateFailure UPV]
  upvPFs = [ AVChangedInPVUpdate (ApName "") (ApVer 0) Nothing
           , ParamsChangedInSVUpdate
           , PVChangedInSVUpdate
           ] ++ (map UPPVVFailure uppvvPFs)
             ++ (map UPSVVFailure upsvvPFs)

  uppvvPFs :: [PredicateFailure UPPVV]
  uppvvPFs = [ CannotFollowPv
             , CannotUpdatePv []
             , AlreadyProposedPv
             ]

  upsvvPFs :: [PredicateFailure UPSVV]
  upsvvPFs = [ AlreadyProposedSv
             , CannotFollowSv
             , InvalidApplicationName
             , InvalidSystemTags
             ]

  upivotesPFs :: [PredicateFailure UPIVOTES]
  upivotesPFs = map (ApplyVotesFailure . UpivoteFailure . UPVOTEFailure . ADDVOTEFailure)
                    addvotePFs
                    {-
                    ([ S_HigherThanThdAndNotAlreadyConfirmed
                     , S_CfmThdNotReached
                     , S_AlreadyConfirmed
                     ])
                    -}

  upiendPFs :: [PredicateFailure UPIEND]
  upiendPFs = map UPENDFailure
                  [ ProtVerUnknown (ProtVer 0 0 0)
                  -- , S_TryNextRule
                  , CanAdopt (ProtVer 0 0 0)
                  , CannotAdopt (ProtVer 0 0 0)
                  , Update.NotADelegate (VKey (Owner 0))
                  , UnconfirmedProposal (UpId 0)
                  ]

  addvotePFs :: [PredicateFailure ADDVOTE]
  addvotePFs = [ AVSigDoesNotVerify
               , NoUpdateProposal (UpId 0)
               ]

  chainPFs :: [PredicateFailure CHAIN]
  chainPFs = concat
    [ map EpochFailure epochPFs
    , [ HeaderSizeTooBig undefined undefined undefined ]
    , map BBodyFailure bbodyPFs
    , map PBFTFailure pbftPFs
    , [ MaximumBlockSize 0 0 ]
    , map LedgerDelegationFailure delegPFs
    , map LedgerUTxOFailure utxowsPFs
    ]

  epochPFs :: [PredicateFailure EPOCH]
  epochPFs = map (UPIECFailure . PVBUMPFailure) []

  bbodyPFs :: [PredicateFailure BBODY]
  bbodyPFs = [ InvalidBlockSize
             , InvalidUtxoHash
             , InvalidDelegationHash
             , InvalidUpdateProposalHash
             ] ++ map BUPIFailure bupiPFs
               ++ map DelegationFailure delegPFs
               ++ map UTXOWSFailure utxowsPFs

  bupiPFs :: [PredicateFailure BUPI]
  bupiPFs = concat [ map UPIREGFailure upiregPFs
                   , map UPIVOTESFailure upivotesPFs
                   , map UPIENDFailure upiendPFs
                   ]

  pbftPFs :: [PredicateFailure PBFT]
  pbftPFs = [ SlotNotAfterLastBlock (Slot 0) (Slot 0)
            , Pbft.SlotInTheFuture (Slot 0) (Slot 0)
            , PrevHashNotMatching (Hash 0) (Hash 0)
            , InvalidHeaderSignature (VKey (Owner 0))
                                     (Sig (Hash 0) (Owner 0))
            ] ++ map SigCountFailure
                     [ TooManyIssuedBlocks (VKeyGenesis (VKey (Owner 0)))
                     , Pbft.NotADelegate
                     ]

-- This is necessary to hide the differing STS types in the list and appease
-- the typechecker.
data WrappedGenSig where
  WrapDELEG    :: Gen (Signal DELEG)    -> WrappedGenSig
  WrapUTXOW    :: Gen (Signal UTXOW)    -> WrappedGenSig
  WrapUTXOWS   :: Gen (Signal UTXOWS)   -> WrappedGenSig
  WrapUPIREG   :: Gen (Signal UPIREG)   -> WrappedGenSig
  WrapUPIVOTES :: Gen (Signal UPIVOTES) -> WrappedGenSig
  WrapCHAIN    :: Gen (Signal CHAIN)    -> WrappedGenSig

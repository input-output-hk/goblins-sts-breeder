{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module BreedingPit
  ( breedStsGoblins
  , genEnvStateSig
  , Gen
  , Population
  , BreederConfig(..)
  , defaultBreederConfig
  )
where

import           Control.Monad.Trans.State.Strict (evalState)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.TypeRepMap as TM
import           Hedgehog
import qualified Hedgehog.Internal.Gen as IGen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as ITree

import           Control.State.Transition
  (IRC(..), STS(..), TRC(..), applyRuleIndifferently)
import           Control.State.Transition.Generator (HasTrace(..))
import           Test.Goblin
import           Moo.GeneticAlgorithm.Binary

data BreederConfig = BreederConfig
  { bcPopSize    :: !Int
  , bcGenomeSize :: !Int
  , bcMaxIters   :: !Int
  , bcEliteCount :: !Int
  , bcGenSize    :: !Int
  } deriving (Show)

defaultBreederConfig :: BreederConfig
defaultBreederConfig = BreederConfig
  { bcPopSize    = 500
  , bcGenomeSize = 4000
  , bcMaxIters   = 100
  , bcEliteCount = 5
  , bcGenSize    = 50
  }

breedStsGoblins
  :: forall sts
   . ( HasTrace sts, Goblin Bool (Signal sts)
     , SeedGoblin (Environment sts), SeedGoblin (State sts))
  => BreederConfig
  -> PredicateFailure sts
  -> IO (Population Bool)
breedStsGoblins breederConfig wantedFailure = do
  genSeed <- Seed.random

  let
    BreederConfig popsize genomeSize maxiters eliteCount genSizeInt
      = breederConfig
    genSize = Size genSizeInt

    -- | Fitness function. This should run the goblins on a set of examples
    -- which we generate atop `initState`.
    fitness :: [Bool] -> Double
    fitness genome = scoreResult $ do -- this is a List monad
      ess <- genEnvStateSig @sts
      let env :: Environment sts
          state :: State sts
          (env, state) = maybe (error "impossible: fitness: env, state") id
                       $ ITree.treeValue
                       . runMaybeT
                       . distributeT
                       . IGen.runGenT genSize genSeed
                       $ (fst <$> ess)

      -- Seed the bagOfTricks
      let seedBagOfTricks = seeder env >> seeder state

      let gd = spawnGoblin genome TM.empty
      let newGenSig = flip evalState gd $ do
                        seedBagOfTricks
                        tinker (snd <$> ess)

      let newSig :: Signal sts
          newSig = maybe (error "impossible: fitness: newSig") id
                 $ ITree.treeValue
                 . runMaybeT
                 . distributeT
                 . IGen.runGenT genSize genSeed
                 $ newGenSig

      let jc = TRC (env, state, newSig)

      -- Apply the signal to the state (and environment)
      tr <- transitionRules
      let (_finalState, pfs) = applyRuleIndifferently @sts jc tr

      pure pfs

     where

      scoreResult :: [[PredicateFailure sts]] -> Double
      scoreResult ls =
        -- 5 for a desired PredicateFailure
        -- --
        -- ^ this objective function must be positive, so
        -- we can't punish unwanted `PredicateFailure`s. We
        -- also add 1 to the function, because otherwise it
        -- seems to stall at 0.
        let failures = concat ls
            goodFailuresCount =
              fromIntegral (length (filter (eqPF wantedFailure)
                                           failures))
        in 5 * goodFailuresCount

    initialize = getRandomBinaryGenomes popsize genomeSize
    select     = stochasticUniversalSampling popsize
    crossover  = onePointCrossover 0.5
    mutate     = pointMutate 0.01
    -- evolve     = loopIO [ TimeLimit 30
    --                     -- , DoEvery 1 (\n pop -> putStrLn $ "gen: " <> show n <> ". " <> show (map snd pop))
    --                     ]
    evolve     = loop (Generations maxiters)-- `Or` converged)
      $ nextGeneration Maximizing fitness select eliteCount crossover mutate
     where
      -- converged =
      --   IfObjective $ \fitvals -> maximum fitvals == minimum fitvals
  population <- runGA initialize evolve
  pure (bestFirst Maximizing population)



genEnvStateSig :: forall sts
                . HasTrace sts
               => [Gen ((Environment sts, State sts), Signal sts)]
genEnvStateSig = do -- below is a List monad
  initRule <- initialRules
  pure $ do
    -- below is a Gen monad
    env <- envGen @sts 10
    let (initState, _predicateFailures) =
          applyRuleIndifferently @sts (IRC env) initRule

    sig <- sigGen @sts Nothing env initState
    pure ((env, initState), sig)

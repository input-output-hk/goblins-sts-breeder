module Parse where

import           Control.Monad (unless)
import qualified Options.Applicative as O
import           System.Directory (doesFileExist)
import           System.Exit (exitFailure)
import           Text.Read (readEither)

import BreedingPit
  (BreederConfig(..), defaultBreederConfig)


-- | Top level parser with info.
opts :: O.ParserInfo (Int, BreederConfig)
opts = O.info (mainParser O.<**> O.helper)
    (  O.fullDesc
    <> O.progDesc desc
    )
 where
  desc = "Goblins breeding executable."
      <> "\n Default values: " <> show defaultBreederConfig

mainParser :: O.Parser (Int, BreederConfig)
mainParser = (,) <$> parseRunCount <*> parseBreederConfig

parseRunCount :: O.Parser Int
parseRunCount =
    O.option O.auto (
            O.long "run_count"
         <> O.short 'r'
         <> O.metavar "COUNT"
         <> O.value 1
         <> O.help "The desired number of runs. Each run trains goblins for each PredicateFailure of each STS rule"
         <> O.showDefault
    )

parseBreederConfig :: O.Parser BreederConfig
parseBreederConfig =
  BreederConfig
    <$> parsePopSize
    <*> parseGenomeSize
    <*> parseMaxIters
    <*> parseEliteCount
    <*> parseGenSize
 where
  parsePopSize :: O.Parser Int
  parsePopSize =
    O.option O.auto (
             O.long "pop_size"
          <> O.short 'p'
          <> O.metavar "POP_SIZE"
          <> O.showDefault
          <> O.value (bcPopSize defaultBreederConfig)
    )
  parseGenomeSize :: O.Parser Int
  parseGenomeSize =
    O.option O.auto (
             O.long "genome_size"
          <> O.short 'g'
          <> O.metavar "GENOME_SIZE"
          <> O.showDefault
          <> O.value (bcGenomeSize defaultBreederConfig)
    )
  parseMaxIters :: O.Parser Int
  parseMaxIters =
    O.option O.auto (
             O.long "max_iters"
          <> O.short 'm'
          <> O.metavar "MAX_ITERS"
          <> O.showDefault
          <> O.value (bcMaxIters defaultBreederConfig)
    )
  parseEliteCount :: O.Parser Int
  parseEliteCount =
    O.option O.auto (
             O.long "elite_count"
          <> O.short 'e'
          <> O.metavar "ELITE_COUNT"
          <> O.showDefault
          <> O.value (bcEliteCount defaultBreederConfig)
    )
  parseGenSize :: O.Parser Int
  parseGenSize =
    O.option O.auto (
             O.long "gen_size"
          <> O.short 's'
          <> O.metavar "GEN_SIZE"
          <> O.showDefault
          <> O.value (bcGenSize defaultBreederConfig)
    )

parseRunIndexFromFile :: FilePath -> IO Int
parseRunIndexFromFile fp = do
  doesExist <- doesFileExist fp
  unless doesExist $ do
    putStrLn "Initializing run index file."
    writeFile fp "0" -- initialize
  eVal <- readEither <$> readFile fp
  ix <- case eVal of
    Left err -> putStrLn ("parseRunIndexFromFile: no parse: " <> err)
             >> exitFailure
    Right ix -> pure ix
  writeFile fp (show (ix+1)) -- increment counter
  pure ix

module ENCS.Opts where

import Options.Applicative

runWithOpts :: IO ServerMode
runWithOpts = execParser $ info (modeParser <**> helper) fullDesc

data ServerMode
    = Run
    | Setup
    | Verify FilePath FilePath
    deriving Show

modeParser :: Parser ServerMode
modeParser = subparser
        (  command "run"    (info (pure Run   )  runDesc)
        <> command "setup"  (info (pure Setup ) setupDesc)
        <> command "verify" (info verifyParser verifyDesc)
        ) 
    where
        runDesc    = progDesc "Default mode without any preliminary work."
        setupDesc  = progDesc "Mint a encs token before running the application."
        verifyDesc = progDesc 
            "Verify file with encs distribution. You can manually pass the names of the input and output files respectively."
        verifyParser = Verify <$> verifyFrom <*> verifyTo
        verifyFrom = strArgument (metavar "FROM" <> help "Distribution file path."          <> value "distribution.json")
        verifyTo   = strArgument (metavar "TO"   <> help "Verified distribution file path." <> value "verified_distribution.json")
module ENCS.Opts where

import Options.Applicative

runWithOpts :: IO ServerMode
runWithOpts = execParser $ info (modeParser <**> helper) fullDesc

data ServerMode = Run | Setup

modeParser :: Parser ServerMode
modeParser = subparser
        (  command "run"   (info (pure Run  ) runDesc)
        <> command "setup" (info (pure Setup) setupDesc)
        )
    where
        runDesc   = progDesc "Default mode without any preliminary work."
        setupDesc = progDesc "Mint a encs token before running the application."
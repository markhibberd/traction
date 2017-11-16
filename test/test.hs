{-# LANGUAGE NoImplicitPrelude #-}

import           Traction.Prelude

import           System.Exit (exitFailure)
import           System.IO (IO)
import qualified System.IO as IO

import qualified Test.Traction


main :: IO ()
main =
  IO.hSetBuffering IO.stdout IO.LineBuffering >> mapM id [
      Test.Traction.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure

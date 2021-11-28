module Main where

import           Data.Either.Utils (unsafeFromEither)
import           Data.Git
import           PipelineWatcher



main :: IO ()
main = stdoutPipelineWatcherWithAutoSelect (GitlabOrigin "test") (unsafeFromEither $ makeCommit "4444444444444444444444444444444444444444")

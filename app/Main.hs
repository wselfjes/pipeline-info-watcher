module Main where

import           Data.Either.Utils (unsafeFromEither)
import           Data.Git
import           PipelineWatcher



main :: IO ()
main = displayPipeline $ watchPipeline (GitlabOrigin "test") (unsafeFromEither $ makeCommit "test")

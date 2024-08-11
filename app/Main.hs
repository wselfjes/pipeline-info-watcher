module Main where

import           Data.Either.Utils (unsafeFromEither)
import           Data.Git
import           PipelineWatcher



main :: IO ()
main = stdoutPipelineWatcherWithAutoSelect (GitlabOrigin "gitlab.com") (Project "andreychertckov/bars_group_test_task") (unsafeFromEither $ makeCommit "bba77b1721ba2ea5288485c487ff49721f33544b")

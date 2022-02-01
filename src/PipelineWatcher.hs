module PipelineWatcher where

import           Data.Git              (Commit, Origin (..), PipelineInfo (..),
                                        Project (..), PipelineId (..))
import qualified Data.Gitlab.ApiReader as GL


selectPipelineIntegration :: Origin -> (Origin -> Project -> Commit -> IO PipelineInfo)
selectPipelineIntegration (GitlabOrigin _) = GL.getPipelineInfo
selectPipelineIntegration (GithubOrigin _) = getPipelineInfo

getPipelineInfo :: Origin -> Project -> Commit -> IO PipelineInfo
getPipelineInfo o p c = return (PipelineInfo o p c (PipelineId 1) "succeded" "www.google.com")

displayPipelineStdout
  :: PipelineInfo
  -> IO ()
displayPipelineStdout pipeline =
  do
    print pipeline


stdoutPipelineWatcherWithAutoSelect
  :: Origin
  -> Project
  -> Commit
  -> IO ()
stdoutPipelineWatcherWithAutoSelect o =
  pipelineWatcher (selectPipelineIntegration o) displayPipelineStdout o


pipelineWatcher
  :: (Monad m)
  => (Origin -> Project -> Commit -> m PipelineInfo)
  -> (PipelineInfo -> m a)
  -> Origin
  -> Project
  -> Commit
  -> m a
pipelineWatcher
 getInfo display o p c =
  do
    info <- getInfo o p c
    display info

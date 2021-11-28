module PipelineWatcher where

import           Data.Git (Commit, Origin (..), PipelineInfo (..))
import qualified Data.Gitlab.ApiReader as GL


selectPipelineIntegration :: Origin -> (Origin -> Commit -> IO PipelineInfo)
selectPipelineIntegration (GitlabOrigin _) = GL.getPipelineInfo
selectPipelineIntegration (GithubOrigin _) = getPipelineInfo

getPipelineInfo :: Origin -> Commit -> IO PipelineInfo
getPipelineInfo o c = return (PipelineInfo o c)

displayPipelineStdout
  :: PipelineInfo
  -> IO ()
displayPipelineStdout pipeline =
  do
    print (getOrigin pipeline)
    print (getCommit pipeline)


stdoutPipelineWatcherWithAutoSelect
  :: Origin
  -> Commit
  -> IO ()
stdoutPipelineWatcherWithAutoSelect o = 
  pipelineWatcher (selectPipelineIntegration o) displayPipelineStdout o


pipelineWatcher
  :: (Monad m)
  => (Origin -> Commit -> m PipelineInfo)
  -> (PipelineInfo -> m a)
  -> Origin
  -> Commit
  -> m a
pipelineWatcher
 getInfo display o c =
  do
    info <-getInfo o c
    display info

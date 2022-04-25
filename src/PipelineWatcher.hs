module PipelineWatcher where

import Control.Concurrent
import System.IO

import           Data.Git              (Commit, Origin (..), PipelineInfo (..),
                                        Project (..), PipelineId (..), Status (..), isTerminal)
import qualified Data.Gitlab.ApiReader as GL


selectPipelineIntegration :: Origin -> (Origin -> Project -> Commit -> IO PipelineInfo)
selectPipelineIntegration (GitlabOrigin _) = GL.getPipelineInfo
selectPipelineIntegration (GithubOrigin _) = getPipelineInfo

getPipelineInfo :: Origin -> Project -> Commit -> IO PipelineInfo
getPipelineInfo o p c = return (PipelineInfo o p c (PipelineId 1) Created "www.google.com")

displayPipelineStdout
  :: PipelineInfo
  -> IO ()
displayPipelineStdout pipeline =
  do
    putStr "\r"
    (putStr . show . getStatus) pipeline
    hFlush stdout


stdoutPipelineWatcherWithAutoSelect
  :: Origin
  -> Project
  -> Commit
  -> IO ()
stdoutPipelineWatcherWithAutoSelect o =
  pipelineWatcher (selectPipelineIntegration o) displayPipelineStdout (threadDelay 1000000) o 


pipelineWatcher
  :: (Monad m)
  => (Origin -> Project -> Commit -> m PipelineInfo)
  -> (PipelineInfo -> m a)
  -> m a
  -> Origin
  -> Project
  -> Commit
  -> m a
pipelineWatcher
 getInfo display timeout o p c =
  do
    timeout
    info <- getInfo o p c
    display info
    if (isTerminal . getStatus) info
    then display info
    else pipelineWatcher getInfo display timeout o p c

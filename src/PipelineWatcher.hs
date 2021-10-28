module PipelineWatcher where

import           Data.Git (Commit, Origin)

data PipelineInfo = PipelineInfo
  { getOrigin :: Origin
  , getCommit :: Commit
  }

watchPipeline
  :: Origin
  -> Commit
  -> IO PipelineInfo
watchPipeline o c =
  do
    return (PipelineInfo o c)


displayPipeline
  :: IO PipelineInfo
  -> IO ()
displayPipeline pipeline =
  do
    (PipelineInfo origin commit) <- pipeline
    print origin
    print commit


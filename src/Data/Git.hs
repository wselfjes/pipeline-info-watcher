{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Git where

import qualified Data.Aeson          as Ae
import qualified Data.Text           as T

import           Data.List.FixedList as Fl
import           Data.String         (IsString (..))

-- $setup
-- >>> :set -XOverloadedStrings

newtype Commit = Commit
  { getCommitId :: FixedList 40 Char
  } deriving (Eq)

newtype ShortCommit = ShortCommit
  { getShortCommitId :: FixedList 7 Char
  }

newtype Project = Project
  { projectId :: String
  } deriving Show

data Origin
  = GithubOrigin
      { address :: String
      }
  | GitlabOrigin
      { address :: String
      }
  deriving (Show)

data Status = Created
            | WaitingForResource
            | Preparing
            | Pending
            | Running
            | Success
            | Failed
            | Canceled
            | Skipped
            | Manual
            | Scheduled deriving Show

isTerminal Created            = False
isTerminal WaitingForResource = False
isTerminal Preparing          = False
isTerminal Pending            = False
isTerminal Running            = False
isTerminal Success            = True
isTerminal Failed             = True
isTerminal Canceled           = True
isTerminal Skipped            = True
isTerminal Manual             = True
isTerminal Scheduled          = False

newtype PipelineId = PipelineId {getPId :: Int} deriving (Ae.FromJSON, Show)

data PipelineInfo = PipelineInfo
  { getOrigin     :: Origin
  , getProject    :: Project
  , getCommit     :: Commit
  , getPipelineId :: PipelineId
  , getStatus     :: Status
  , getWebUrl     :: String
  } deriving Show


instance IsString Commit where
  fromString = Commit . fromString

instance Ae.FromJSON Commit where
  parseJSON = Ae.withText "commit" $ return . (fromString . T.unpack)

instance (Show Commit) where
  show = show . Fl.getFixedList . getCommitId

instance (Show ShortCommit) where
  show = show . Fl.getFixedList . getShortCommitId

-- | Safe make commit out of string.
--
-- >>> makeCommit "ashtioashnt"
-- Left "expected list size 40 but input list is of size 11"
-- >>> makeCommit "7dc8ed0f0620c92962b33e52b0f877bb5d4d2786"
-- Right "7dc8ed0f0620c92962b33e52b0f877bb5d4d2786"
makeCommit :: String -> Either String Commit
makeCommit = fmap Commit . listToFixedList

-- | Make short commit out of commit.
--
-- >>> shortCommit ("7dc8ed0f0620c92962b33e52b0f877bb5d4d2786" :: Commit)
-- "7dc8ed0"
shortCommit :: Commit -> ShortCommit
shortCommit = ShortCommit . Fl.take . getCommitId

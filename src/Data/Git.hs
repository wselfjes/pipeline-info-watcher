{-# LANGUAGE DataKinds #-}

module Data.Git where

import           Data.List.FixedList as Fl
import           Data.String         (IsString (..))

-- $setup
-- >>> :set -XOverloadedStrings

newtype Commit = Commit
  { getCommitId :: FixedList 40 Char
  }

newtype ShortCommit = ShortCommit
  { getShortCommitId :: FixedList 7 Char
  }

data Origin
  = GithubOrigin
      { address :: String
      }
  | GitlabOrigin
      { address :: String
      }
  deriving (Show)

data PipelineInfo = PipelineInfo
  { getOrigin :: Origin
  , getCommit :: Commit
  }


instance IsString Commit where
  fromString = Commit . fromString

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

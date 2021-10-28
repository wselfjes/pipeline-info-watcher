{-# LANGUAGE DataKinds #-}

module Data.Git where

import           Data.List.FixedList as Fl
import           Data.String       (IsString (..))

-- $setup
-- >>> :set -XOverloadedStrings

newtype Commit = Commit
  { getCommitId :: FixedList 40 Char
  }

instance IsString Commit where
  fromString = Commit . fromString

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

instance (Show Commit) where
  show = show . Fl.getFixedList . getCommitId

instance (Show ShortCommit) where
  show = show . Fl.getFixedList . getShortCommitId

makeCommit :: String -> Either String Commit
makeCommit = fmap Commit . listToFixedList

-- | Make short commit out of commit
--
-- >>> shortCommit ("7dc8ed0f0620c92962b33e52b0f877bb5d4d2786" :: Commit)
-- "7dc8ed0"
shortCommit :: Commit -> ShortCommit
shortCommit = ShortCommit . Fl.take . getCommitId

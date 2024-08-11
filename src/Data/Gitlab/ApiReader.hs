{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Gitlab.ApiReader where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson             as Ae
import           Data.Scientific
import qualified Data.Text              as T
import           Network.HTTP.Req


import qualified Data.Git as Git

instance FromJSON Git.Status where
  parseJSON = withText "status" $ \v ->
    do
      case v of
        "created"              -> return Git.Created
        "waiting_for_resource" -> return Git.WaitingForResource
        "preparing"            -> return Git.Preparing
        "pending"              -> return Git.Pending
        "running"              -> return Git.Running
        "success"              -> return Git.Success
        "failed"               -> return Git.Failed
        "canceled"             -> return Git.Canceled
        "skipped"              -> return Git.Skipped
        "manual"               -> return Git.Manual
        "scheduled"            -> return Git.Scheduled
        _ -> fail "must be one of [created, waiting_for_resource, preparing, pending, running, success, failed, canceled, skipped, manual, scheduled]"

data RawGitlabPipeline = RawGitlabPipeline {sha :: Git.Commit, pid :: Git.PipelineId, status :: Git.Status, webUrl :: String} deriving Show


instance FromJSON RawGitlabPipeline where
  -- parseJSON :: Value -> Parser RawGitlabPipeline
  parseJSON = withObject "rawGitlabPipeline" $ \o
    -> RawGitlabPipeline
    <$> o .: "sha"
    <*> o .: "id"
    <*> o .: "status"
    <*> o .: "web_url"

getPipelineInfo :: Git.Origin -> Git.Project -> Git.Commit -> IO Git.PipelineInfo
getPipelineInfo o p c = do
  commitsPipeline <- head <$> fmap (filter (\x -> sha x == c)) (getAllPipelines o p)
  return $ Git.PipelineInfo o p c (pid commitsPipeline) (status commitsPipeline) (webUrl commitsPipeline)


getAllPipelines :: Git.Origin -> Git.Project -> IO [RawGitlabPipeline]
getAllPipelines o p = do
  link <- liftIO (makeLinkGetPipelines o p)
  runReq defaultHttpConfig $ do
    r <-
      req
        GET -- method
        link
        NoReqBody
        jsonResponse -- specify how to interpret response
        mempty -- query params, headers, explicit port number, etc.
    return (responseBody r)


makeLinkGetPipelines :: Git.Origin -> Git.Project -> IO (Url Https)
makeLinkGetPipelines (Git.GitlabOrigin o) project = return (https (T.pack o)/: "api" /: "v4" /: "projects" /: T.pack (Git.projectId project) /: "pipelines")
makeLinkGetPipelines _ _ = error "not gitlab origin"

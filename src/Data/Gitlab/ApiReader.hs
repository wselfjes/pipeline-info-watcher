{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Gitlab.ApiReader where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Scientific
import qualified Data.Text              as T
import           Network.HTTP.Req


import           Data.Git



data RawGitlabPipeline = RawGitlabPipeline {sha :: Commit, pid :: PipelineId, status :: String, webUrl :: String} deriving Show

instance FromJSON RawGitlabPipeline where
  -- parseJSON :: Value -> Parser RawGitlabPipeline
  parseJSON = withObject "rawGitlabPipeline" $ \o
    -> RawGitlabPipeline
    <$> o .: "sha"
    <*> o .: "id"
    <*> o .: "status"
    <*> o .: "web_url"

getPipelineInfo :: Origin -> Project -> Commit -> IO PipelineInfo
getPipelineInfo o p c = do
  commitsPipeline <- head <$> fmap (filter (\x -> sha x == c)) (getAllPipelines o p)
  return $ PipelineInfo o p c (pid commitsPipeline) (status commitsPipeline) (webUrl commitsPipeline)


getAllPipelines :: Origin -> Project -> IO [RawGitlabPipeline]
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


makeLinkGetPipelines :: Origin -> Project -> IO (Url Https)
makeLinkGetPipelines (GitlabOrigin o) project = return (https (T.pack o)/: "api" /: "v4" /: "projects" /: T.pack (projectId project) /: "pipelines")
makeLinkGetPipelines _ _ = error "not gitlab origin"

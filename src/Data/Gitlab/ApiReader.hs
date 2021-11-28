{-# LANGUAGE OverloadedStrings #-}
module Data.Gitlab.ApiReader where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req


import           Data.Git


newtype PipelineId = PipelineId {getPId :: Int}

getPipelineInfo :: Origin -> Commit -> IO PipelineInfo
getPipelineInfo o c = do
  pipelineId <- getPipelineId o c
  readPipelineInfo o c pipelineId


getPipelineId :: Origin -> Commit -> IO PipelineId
getPipelineId o c = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.

  liftIO $ print (responseBody r :: Value)
  return (PipelineId 1)


readPipelineInfo :: Origin -> Commit -> PipelineId -> IO PipelineInfo
readPipelineInfo o c _ = return (PipelineInfo o c)


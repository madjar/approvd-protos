{-# LANGUAGE OverloadedStrings #-}
module PullRequest (PullRequest (..), issueComments, pullRequest, ourStatus, newStatus) where

import Control.Lens ((^.))
import Control.Monad (mfilter)
import Data.Aeson.Lens (key, _String, _Array, _Value)
import Data.Aeson (object, (.=), Value, Array)
import Data.Foldable (find)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import Github (Github, getRepo, postRepo)


data PullRequest = PullRequest { getPR :: L.ByteString
                               , getSha :: String
                               , getRelevantComments :: Github Array }

context :: String
context = "approvd-haskell-proto"

-- | Return the comments of an issue
issueComments :: Int -> Github L.ByteString
issueComments issue = getRepo ["issues", show issue, "comments"]

-- | Return a pull request
pullRequest :: Int -> Github PullRequest
pullRequest prId = do
  pr <- getRepo ["pulls", show prId]
  let sha = T.unpack $ pr ^. key "head" . key "sha" . _String
      push_date = pr ^. key "head" . key "pushed_at" . _String -- TODO parse as date
      relevant issue = issue ^. key "created_at" . _String > push_date
      relevantComments = do comments <- issueComments prId
                            return $ mfilter relevant (comments ^. _Array)
  return $ PullRequest pr sha relevantComments


-- | Return the status we previously gave to a commit
ourStatus :: String -> Github (Maybe Value)
ourStatus sha = do
  status <- getRepo ["status", sha]
  return $ find ours (status ^. key "statuses" . _Array)
    where ours s = s ^. key "context" . _String == T.pack context

-- | Changes the status of a commit
newStatus :: String               -- | Sha of the commit
          -> String               -- | State
          -> String               -- | Description
          -> Github L.ByteString
newStatus sha state description =
  postRepo ["statuses", sha] $ object [ "state" .= state
                                      , "description" .= description
                                      , "context" .= context ]

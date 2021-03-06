{-# LANGUAGE OverloadedStrings #-}
module PullRequest (PullRequest (..), issueComments, pullRequest, ourStatus, postStatus, handlePR) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad (mfilter)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson (object, (.=), Value, Array)
import Data.Foldable (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Text.Regex.PCRE ((=~))

import Github (Github, getRepo, postRepo)

import Debug.Trace

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
  let sha = pr ^. key "head" . key "sha" . _String
  -- TODO parse as date
  pushDate <- commitPushDate sha
  let relevant comment = comment ^. key "created_at" . _String > pushDate
      relevantComments = do comments <- issueComments prId
                            return $ mfilter relevant (comments ^. _Array)
  return $ PullRequest pr (T.unpack sha) relevantComments

-- To known when a commit was pushed, I should have a webhook and listend for pull_request events with the synchronized action. Otherwise, I have to resort to the following hack.
commitPushDate :: T.Text -> Github T.Text
commitPushDate sha =
  do events <- getRepo ["events"]
     let event = V.find f (events ^. _Array)
     case event of
      Just e -> return $ e ^. key "created_at" . _String
      Nothing -> trace ("XXX: No date found for " ++ T.unpack sha) $ return ""
  where f event = isPushEvent event && eventSha event == sha
        isPushEvent event = event ^. key "type" . _String == "PushEvent"
        eventSha event = event ^. key "payload" . key "head" . _String

-- | Return the status we previously gave to a commit
ourStatus :: String -> Github (Maybe Value)
ourStatus sha = do
  status <- getRepo ["status", sha]
  return $ find ours (status ^. key "statuses" . _Array)
    where ours s = s ^. key "context" . _String == T.pack context

-- | Changes the status of a commit
postStatus :: String               -- | Sha of the commit
           -> String               -- | State
           -> String               -- | Description
           -> Github L.ByteString
postStatus sha state description =
  postRepo ["statuses", sha] $ object [ "state" .= state
                                      , "description" .= description
                                      , "context" .= context ]

-- | Do stuff with a pull request.  Honesly, we just do random stuff
-- to check the rest work. Here, for example, we set the status of the
-- last commit of the pull request to pending if it's not already set
handlePR :: Int -> Github (String, String, Bool)
handlePR pr =
  do pull <- pullRequest pr
     statusObj <- ourStatus $ getSha pull
     comments <- getRelevantComments pull
     let status = T.unpack . (^. key "state" . _String) <$> statusObj
         message = T.unpack . (^. key "description" . _String) <$> statusObj
         totalComments = V.length comments
         approvingComments = V.length $ mfilter isApproval comments
         newStatus = if approvingComments > 0
                     then "success"
                     else "pending"
         newMessage = "Seen " ++ show totalComments ++ " relevant comments, with " ++ show approvingComments ++ " approvals."

     case (status, message) of
      (Just s, Just m) | s == newStatus && m == newMessage -> return (s, m, False)
      _ -> postStatus (getSha pull) newStatus newMessage
           >> return (newStatus, newMessage, True)


isApproval :: Value -> Bool
isApproval comment = body =~ pattern
  where pattern = "^[Aa]pprove?d" :: B.ByteString
        body = encodeUtf8 $ comment ^. key "body" . _String

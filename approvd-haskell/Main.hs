{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad (mfilter)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Text.Regex.PCRE ((=~))

import Debug.Trace

import Github (Github, runGithub)
import PullRequest (PullRequest (..), pullRequest, ourStatus, postStatus)

auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"


-- | Do stuff with a pull request.  Honesly, we just do random stuff
-- to check the rest work. Here, for example, we set the status of the
-- last commit of the pull request to pending if it's not already set
handlePR :: Int -> Github (String, Bool)
handlePR pr =
  do pull <- pullRequest pr
     statusObj <- ourStatus $ getSha pull
     comments <- getRelevantComments pull
     let status = T.unpack . (^. key "state" . _String) <$> statusObj
         totalComments = V.length comments
         approvingComments = V.length $ mfilter isApproval comments
         newStatus = if approvingComments > 0
                     then "success"
                     else "pending"
         message = "Seen " ++ show totalComments ++ " relevant comments, with " ++ show approvingComments ++ " approvals."
     traceShowM message

     case status of
      Just s | s == newStatus -> return (s, False)
      _ -> postStatus (getSha pull) newStatus message
           >> return (newStatus, True)


isApproval :: Value -> Bool
isApproval comment = body =~ pattern
  where pattern = "^[Aa]pprove?d" :: B.ByteString
        body = encodeUtf8 $ comment ^. key "body" . _String


main :: IO ()
main = do token <- auth
          r <- runGithub token "madjar" "approvd-protos" $ handlePR  1
          print r

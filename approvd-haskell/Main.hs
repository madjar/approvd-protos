{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^.), (^?))
import Control.Monad (mfilter, when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _String, _Number)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Text.Regex.PCRE ((=~))

import Github (Github, runGithub)
import PullRequest (PullRequest (..), pullRequest, ourStatus, postStatus)

import Web.Scotty

auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"


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


main :: IO ()
main = do token <- auth
          scotty 3000 $ do
            post "/payload" $ do
              eventType <- header "X-Github-Event"
              when (eventType == Just "issue_comment") $ do
                -- TODO : check it is indeed a PR
                body <- jsonData
                let prNumber = truncate $ fromJust $ (body::Value) ^? key "issue" . key "number" . _Number
                result <- liftIO $ runGithub token "madjar" "approvd-protos" $ handlePR prNumber
                liftIO $ putStrLn $ show prNumber ++ " : " ++ show result

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^?))
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Number)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)

import Github (runGithub)
import PullRequest (handlePR)

import Web.Scotty

auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"




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

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import Github (Github, runGithub)
import PullRequest (PullRequest (..), pullRequest, ourStatus, newStatus)

auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"


-- | Do stuff with a pull request.  Honesly, we just do random stuff
-- to check the rest work. Here, for example, we set the status of the
-- last commit of the pull request to pending if it's not already set
handlePR :: Int -> Github String
handlePR pr =
  do pull <- pullRequest pr
     status <- ourStatus $ getSha pull
     case status of
      Just s -> return $ T.unpack $ s ^. key "state" . _String
      Nothing -> L.unpack <$> newStatus (getSha pull) "pending" "Haskell approves too"


main :: IO ()
main = do token <- auth
          r <- runGithub token "madjar" "approvd-protos" $ handlePR  1
          putStrLn r

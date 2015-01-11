{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^.), (^?))
import Control.Monad (mfilter)
import Data.Aeson (object, (.=), Value)
import Data.Aeson.Lens (key, _String, _Array)
import Data.Foldable (find)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T

import Github (Github, runGithub, get, post)

auth :: IO (Maybe B.ByteString)
auth = Just <$> B.init <$> B.readFile "../token"

context :: String
context = "approvd-haskell-proto"

-- | Return the comments of an issue
issueComments :: String -> String -> Int -> Github L.ByteString
issueComments user repo issue =
  get ["repos", user, repo, "issues", show issue, "comments"]

-- | Return a pull request
pullRequest :: String -> String -> Int -> Github L.ByteString
pullRequest user repo pr =
  get ["repos", user, repo, "pulls", show pr]

-- | Return the status we previously gave to a commit
ourStatus :: String -> String -> String -> Github (Maybe Value)
ourStatus user repo sha = do
  status <- get ["repos", user, repo, "status", sha]
  return $ find ours (status ^. key "statuses" . _Array)
    where ours s = s ^. key "context" . _String == T.pack context

-- | Changes the status of a commit
newStatus
  :: String -> String -> String -> String -> String -> Github L.ByteString
newStatus user repo sha state description =
  post ["repos", user, repo, "statuses", sha] $
    object [ "state" .= state
           , "description" .= description
           , "context" .= context ]

-- | Do stuff with a pull request.  Honesly, we just do random stuff
-- to check the rest work. Here, for example, we set the status of the
-- last commit of the pull request to pending if it's not already set
handlePR :: String -> String -> Int -> Github String
handlePR user repo pr =
  do pull <- pullRequest user repo pr
     let sha = T.unpack $ pull ^. key "head" . key "sha" . _String
     status <- ourStatus user repo sha
     case status of
      Just s -> return $ T.unpack $ s ^. key "state" . _String
      Nothing -> L.unpack <$> newStatus user repo sha "pending" "Haskell approves too"


main = do token <- auth
          r <- runGithub token $ handlePR "madjar" "approvd-protos" 1
          putStrLn r

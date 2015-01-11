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

import Github (Github, runGithub, getRepo, postRepo)

auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"

context :: String
context = "approvd-haskell-proto"

-- | Return the comments of an issue
issueComments :: Int -> Github L.ByteString
issueComments issue = getRepo ["issues", show issue, "comments"]

-- | Return a pull request
pullRequest :: Int -> Github L.ByteString
pullRequest pr = getRepo ["pulls", show pr]

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

-- | Do stuff with a pull request.  Honesly, we just do random stuff
-- to check the rest work. Here, for example, we set the status of the
-- last commit of the pull request to pending if it's not already set
handlePR :: Int -> Github String
handlePR pr =
  do pull <- pullRequest pr
     let sha = T.unpack $ pull ^. key "head" . key "sha" . _String
     status <- ourStatus sha
     case status of
      Just s -> return $ T.unpack $ s ^. key "state" . _String
      Nothing -> L.unpack <$> newStatus sha "pending" "Haskell approves too"

main = do token <- auth
          r <- runGithub token "madjar" "approvd-protos" $ handlePR  1
          putStrLn r

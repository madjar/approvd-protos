{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Github (Github, Auth, Token, runGithub, get, post) where

import Control.Applicative (Applicative)
import Control.Lens ((^.), (&), (?~), (.~))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import qualified Network.Wreq as W
import Network.Wreq.Types (Postable)


type Token = B.ByteString
type Auth = Maybe Token

-- | A monad that call the github api with a given authentication
newtype Github a = Github { runG :: ReaderT Auth IO a }
                 deriving (Functor, Monad, Applicative)

-- | Run Github with a given auth and extract the final value
runGithub :: Auth -> Github a -> IO a
runGithub t g = runReaderT (runG g) t

-- | Run a get request to a github endpoint in the Github monad
get :: [String] -> Github L.ByteString
get = request W.getWith

-- | Run a post request to a github endpoint in the Github monad
post :: Postable a => [String] -> a -> Github L.ByteString
post url p = request postWith' url
  where postWith' opts url = W.postWith opts url p


type RequestF = W.Options -> String -> IO (W.Response L.ByteString)

githubUrl = "https://api.github.com"

request :: RequestF -> [String] -> Github L.ByteString
request method components = Github g
  where g = do
          let url = intercalate "/" (githubUrl:components)
          auth <- ask
          r <- liftIO $ method (options auth) url
          return $ r ^. W.responseBody

userAgent = "Approvd.io bot/0.1 (@madjar)"

options :: Auth -> W.Options
options auth = authopts & W.header "User-Agent" .~ [userAgent]
  where authopts = case auth of
                    Just t -> W.defaults & W.auth ?~ W.oauth2Token t
                    Nothing -> W.defaults
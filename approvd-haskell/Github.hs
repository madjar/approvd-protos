{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Github (Github, runGithub, get, post, getRepo, postRepo) where

import Control.Applicative (Applicative)
import Control.Lens ((^.), (&), (?~), (.~))
import Control.Monad ((>=>))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import qualified Network.Wreq as W
import Network.Wreq.Types (Postable)


data GithubConfig = GithubConfig { gToken :: B.ByteString
                                 , gUser :: String
                                 , gRepo :: String }

-- | A monad that call the github api with a given authentication
newtype Github a = Github { runG :: ReaderT GithubConfig IO a }
                 deriving (Functor, Monad, Applicative)

-- | Run Github with a given auth and extract the final value
runGithub :: B.ByteString  -- ^ Authentication token
          -> String        -- ^ Base user to consider
          -> String        -- ^ Base repo
          -> Github a      -- ^ The Github action to run
          -> IO a
runGithub token user repo g = runReaderT (runG g) (GithubConfig token user repo)

-- | Run a get request to a github endpoint in the Github monad
get :: [String] -> Github L.ByteString
get = request W.getWith

-- | Run a post request to a github endpoint in the Github monad
post :: Postable a => [String] -> a -> Github L.ByteString
post url p = request postWith' url
  where postWith' opts url' = W.postWith opts url' p

-- | Get a url relative to the repo
getRepo :: [String] -> Github L.ByteString
getRepo = repoUrl >=> get

-- | Post to a url relative to the repo
postRepo :: Postable a => [String] -> a -> Github L.ByteString
postRepo url p = do url' <- repoUrl url
                    post url' p

-- | Constructs an url where the rooted at the repo api endpoint
repoUrl :: [String] -> Github [String]
repoUrl url = do config <- Github ask
                 return $ "repos":gUser config:gRepo config:url

type RequestF = W.Options -> String -> IO (W.Response L.ByteString)

githubUrl :: String
githubUrl = "https://api.github.com"

request :: RequestF -> [String] -> Github L.ByteString
request method components = Github $ do
          let url = intercalate "/" (githubUrl:components)
          config <- ask
          r <- liftIO $ method (options $ gToken config) url
          return $ r ^. W.responseBody

userAgent :: B.ByteString
userAgent = "Approvd.io bot/0.1 (@madjar)"

options :: B.ByteString -> W.Options
options token = W.defaults & W.header "User-Agent" .~ [userAgent]
                           & W.auth ?~ W.oauth2Token token

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (^.), (^?))
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, _Number, _String)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Data.Text.Lazy as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.Wreq as W
import Web.Scotty
import Web.Cookie
import Web.Scotty.Cookie
import Debug.Trace
import Data.Monoid (mconcat)

import qualified Github as G
import PullRequest (handlePR)


auth :: IO B.ByteString
auth = B.init <$> B.readFile "../token"

clientId :: B.ByteString
clientId = "b0390cb88607d80378d7"

clientSecret :: B.ByteString
clientSecret = "6a42c43203623ec454d75d4a8639ff18ca4baf0a"

secretState :: B.ByteString
secretState = "randomState"  -- XXX Make this a real random string

githubAuthorize :: T.Text
githubAuthorize = "https://github.com/login/oauth/authorize" `T.append` qs
  where qs = T.fromStrict . decodeUtf8 . renderSimpleQuery True $
               [ ("client_id", clientId)
               , ("scope", "write:repo_hook,repo")
               , ("state", secretState) ]


main :: IO ()
main = do token <- auth
          scotty 3000 $ do
            get "/" $ do
              token <- getCookie "github_token"
              user <- case token of
                Just t -> do r <- liftIO $ G.runGithub (encodeUtf8 t) "" "" $ G.get ["user"]
                             return $ r ^? key "name" . _String
                Nothing -> return Nothing
              html $ renderHtml $(shamletFile "index.hamlet")
            get "/login" $
              redirect githubAuthorize
            get "/login/redirect" $ do
              code <- param "code"
              state <- param "state"
              when (state /= secretState) $ do
                status badRequest400
                html "NOPE"
              let opts = W.defaults & W.header "Accept" .~ ["application/json"]
                  url = "https://github.com/login/oauth/access_token"
                  dat = [ ("client_id" :: B.ByteString, clientId)
                        , ("client_secret", clientSecret)
                        , ("code", code) ]
              r <- liftIO $ W.postWith opts url dat
              let githubToken = r ^. W.responseBody . key "access_token" . _String
              setCookie $  (makeSimpleCookie "github_token" githubToken) { setCookiePath = Just "/" }
              redirect "/"
            post "/payload" $ do
              eventType <- header "X-Github-Event"
              when (eventType == Just "issue_comment") $ do
                -- TODO : check it is indeed a PR
                body <- jsonData
                let prNumber = truncate $ fromJust $ (body::Value) ^? key "issue" . key "number" . _Number
                result <- liftIO $ G.runGithub token "madjar" "approvd-protos" $ handlePR prNumber
                liftIO $ putStrLn $ show prNumber ++ " : " ++ show result

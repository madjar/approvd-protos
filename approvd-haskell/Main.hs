{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString as B

import Github (runGithub, get)


auth = Just <$> B.init <$> B.readFile "../token"

main = do token <- auth
          r <- runGithub token $ get ["repos", "madjar", "approvd-protos"]
          print r
          --print $ r ^. key "login" . _String

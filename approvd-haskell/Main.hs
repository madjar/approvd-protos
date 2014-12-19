import Text.Regex.PCRE ((=~))
import Github.Auth (GithubAuth(..))

import qualified Github.Issues.Comments as Github

auth :: Maybe GithubAuth
auth  = Just $ GithubOAuth "77f1658f80998ba6c540981706ef304a422ba60a"

getComments :: Int -> IO (Either Github.Error [Github.IssueComment])
getComments = Github.comments' auth "madjar" "approvd-protos"


handlePullRequest :: Int -> IO ()
handlePullRequest pr = do approved <- getApproved pr
                          if approved
                            then markApproved pr
                            else print $ show pr ++ " is not approved"

getApproved :: Int -> IO Bool
getApproved pr = do possibleComments <- getComments pr
                    case possibleComments of
                     (Left error) -> fail $ show error
                     (Right comments) -> return $ isApproved comments

isApproved :: [Github.IssueComment] -> Bool
isApproved comments = any bodyIsApproved comments
  where bodyIsApproved c = (Github.issueCommentBody c) =~ "^[aA]pprove?d"

markApproved :: Int -> IO ()
markApproved pr = print $ show pr ++ " is approved"

main = handlePullRequest 1

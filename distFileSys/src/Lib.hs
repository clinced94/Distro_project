{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Time.Calendar
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant


data User = User
  { userId      :: Int
  , userName    :: String
  , email       :: String
  , userRegDate :: Day
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserApi = "users" :> Get '[JSON] [User]
                :<|> "albert" :> Get '[JSON] User
                :<|> "isaac" :> Get '[JSON] User

startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac

users :: [User]
users = [ isaac, albert]

isaac :: User
isaac = User 372 "Isaac Newton" "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User 136 "Albert Einstein" "ae@mc2.org" (fromGregorian 1905 12 1)

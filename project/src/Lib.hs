{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.Compat
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString               (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media            ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Utf8


data User = User
  { userId        :: Int
  , userFirstName :: String
  , userEmail     :: String
  , userJoined    :: Day
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = "users" :> Get '[JSON] [User]
            :<|> "albert" :> Get '[JSON] User
            :<|> "isaac" :> Get '[JSON] User
            :<|> "sortedById" :> Get '[JSON] [USER]

startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080"
    run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac
    :<|> return sortedById

users :: [User]
users = [albert, isaac]

albert :: User
albert = User 42 "Albert Einstein" "Albert.E.MC.squared@gmail.com" (fromGregorian 1909 5 8)

isaac :: User
isaac = User 97 "Isaac Newton" "IsaacAttack@hotmail.com" (fromGregorian 1668 7 3)

sortByID :: [User] -> [User]
sortByID = sortBy (comparing userId)

sortedById :: [User]
sortedById = sortByID users

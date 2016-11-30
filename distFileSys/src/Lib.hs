{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Lib
    ( startApp
    ) where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.TH
import           Data.List                (sortBy)
import           Data.Ord                 (comparing)
import           Data.Time.Calendar
import           Database.MongoDB         (Action, Document, Value, access,
                                           allCollections, close, connect,
                                           delete, exclude, find, findOne, host,
                                           insert, insertMany, master, project,
                                           rest, select, sort, (=:))
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

type UserAPI = "users" :> Get '[JSON] [User]
                :<|> "albert" :> Get '[JSON] User
                :<|> "isaac" :> Get '[JSON] User
                :<|> "sortedById" :> Get '[JSON] [User]

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
    :<|> return sortedById

users :: [User]
users = [ isaac, albert]

isaac :: User
isaac = User 372 "Isaac Newton" "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User 136 "Albert Einstein" "ae@mc2.org" (fromGregorian 1905 12 1)

sortedById :: [User]
sortedById = sortById users

sortById :: [User] -> [User]
sortById = sortBy (comparing userId)


--Base function for all DB functionality
--takes in function (e.g. insert, delete, find, etc.)
--and then accesses the DB and applies that function to it.
runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "test" functionToRun
    print e
    close pipe

printData = runMongo allCollections

findFirstFile = runMongo $ findOne $ select [] "posts"

findAllFiles = runMongo $ find (select [] "posts") >>= rest

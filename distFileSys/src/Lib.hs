{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import           Control.Monad.Trans        (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Attoparsec.ByteString
import           Data.Bson.Generic
import           Data.List                  (sortBy)
import           Data.Maybe                 (mapMaybe)
import           Data.Ord                   (comparing)
import           Data.Time.Calendar
import           Database.MongoDB           (Action, Document, Value, access,
                                             allCollections, close, connect,
                                             delete, exclude, find, findOne,
                                             host, insert, insertMany, master,
                                             project, rest, select, sort, (=:))
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant


data User = User
  { username :: String
  , password :: String
  } deriving (Eq, Show, Generic, Read, FromBSON, ToBSON, FromJSON, ToJSON)


data TheFile = TheFile
    { theContents :: String
    } deriving (Show, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String
deriving instance ToBSON String

data ResponseData = ResponseData
    { response :: String
    } deriving Generic

instance ToJSON ResponseData
instance FromJSON ResponseData

type UserAPI = "saveFile" :> ReqBody '[JSON] TheFile :>
                        Post '[JSON] ResponseData

startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = saveFile


--Core function for DB functionality
runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "fileCabinet" functionToRun
    print e
    close pipe

returnMongo :: Action IO a0 -> IO a0
returnMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "fileCabinet" functionToRun
    close pipe
    return e

findFirstFile = runMongo $ findOne $ select [] "files"

findAllFiles = runMongo $ find (select [] "files") >>= rest

insertFile :: Document -> IO ()
insertFile inFile = runMongo $ insert "files" inFile

deleteFile :: Document -> IO ()
deleteFile delFile = runMongo $ delete $ select delFile "files"

saveFile :: TheFile -> Handler ResponseData
saveFile theFile = liftIO $ do
    e <- insertFile ( toBSON theFile )
    return $ ResponseData (theContents theFile)


findUsersByName :: Maybe String -> Handler [User]
findUsersByName username = liftIO $ do
    docs <- returnMongo $ find (select ["username" =: username] "users") >>= rest
    return $ mapMaybe (\ b -> fromBSON b :: Maybe User) docs

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

import           Control.Monad.Trans      (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.Char
import           Data.Maybe
import           Data.String
import           Data.Time.Calendar
import           Database.MongoDB         (Action, Document, Value, access,
                                           allCollections, close, connect,
                                           delete, exclude, find, findOne, host,
                                           insert, insertMany, master, project,
                                           rest, select, sort, (=:))
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           System.Random

data Key = Key
    {key :: Int
    } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)


data Token = Token
    { token :: Int
    } deriving (Show, Read)

$(deriveJSON defaultOptions ''Token)

keysList = [1..25]

--authentication code
generateKey :: IO Int
generateKey = randomRIO(1, 25)


--test keys
publicKey :: Key
publicKey = Key 7

getPublicKey :: Key
getPublicKey = publicKey


privateKey :: Key
privateKey = Key 4

getPrivateKey :: Key
getPrivateKey = privateKey

token3 :: Token
token3 = Token 3

caesarEncrypt :: Int -> String -> String
caesarEncrypt theKey theFile =
    let ords = map ord theFile
        encrypted = map (+ theKey) ords
    in map chr encrypted

caesarDecrypt :: Int -> String -> String
caesarDecrypt theKey theFile = caesarEncrypt (negate theKey) theFile



login :: Maybe String -> Handler Token
login username = liftIO $ do
    retrieveUsers username
    return token3



--database interaction

data TheFile = TheFile
    { theContents :: String
    } deriving (Show, Read, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String
deriving instance ToBSON String

data ResponseData = ResponseData
    { response :: String
    } deriving (Generic)

instance ToJSON ResponseData
instance FromJSON ResponseData

data User = User
  { username :: String
  , password :: String
  } deriving (Eq, Show, Generic, Read, FromBSON, ToBSON, FromJSON, ToJSON)

-- $(deriveJSON defaultOptions ''User)


type LoginAPI = "login" :> QueryParam "username" String :> Get '[JSON] Token


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server


api :: Proxy LoginAPI
api = Proxy

server :: Server LoginAPI
server = login


runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "fileCabinet" functionToRun
    print e
    close pipe



-- printData = runMongo allCollections

-- findFirstFile = runMongo $ findOne $ select [] "files"

-- findAllFiles = runMongo $ find (select [] "files") >>= rest

findAllUsers = runMongo $ find (select [] "users")  >>= rest

insertFile :: Document -> IO ()
insertFile inFile = runMongo $ insert "files" inFile

insertUser :: Document -> IO ()
insertUser inUser = runMongo $ insert "users" inUser

deleteFile :: Document -> IO ()
deleteFile delFile = runMongo $ delete $ select delFile "files"

deleteUser :: Document -> IO ()
deleteUser delUser = runMongo $ delete $ select delUser "users"


addUser :: User -> IO ResponseData
addUser theUser = liftIO $ do
    let theName = username theUser
    --putStrLn ("Username: " ++ theName)
    let encryptedPassword = caesarEncrypt (key privateKey) (password theUser)
    --putStrLn ("Encrypted password: " ++ encryptedPassword)
    let userToAdd = User theName encryptedPassword
    e <- insertUser ( toBSON userToAdd)
    --putStrLn "User added successfully"
    return $ ResponseData (username userToAdd)


retrieveUsers :: Maybe String -> IO ( Either String [User] )
retrieveUsers username = do
    manager <- newManager defaultManagerSettings
    results <- runClientM (findUsersByName username) (ClientEnv manager (BaseUrl Http "127.0.0.1" 8080 ""))
    return $ case results of
        Left err    -> Left ("An error has occurred: " ++ show err)
        Right users -> Right users




type DataAPI = "findUsersByName" :> QueryParam "search" String :> Get '[JSON] [User]
                :<|> "saveFile" :> ReqBody '[JSON] TheFile :> Post '[JSON] ResponseData


dataAPI :: Proxy DataAPI
dataAPI = Proxy

(findUsersByName :<|> saveFile) = client dataAPI




--curl -X POST -d '{"file": "123"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/saveFile

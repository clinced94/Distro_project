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


--authentication code
generateKey :: IO Int
generateKey = randomRIO(1,25)

--test Keys
publicKey :: Key
publicKey = Key 13

privateKey :: Key
privateKey = Key 2

token11 :: Token
token11 = Token 11

caesarEncrypt :: Int -> String -> String
caesarEncrypt theKey theFile =
    let ords = map ord theFile
        encrypted = map (+ theKey) ords
    in map chr encrypted

caesarDecrypt :: Int -> String -> String
caesarDecrypt theKey theFile = caesarEncrypt (negate theKey) theFile



login :: Maybe String -> Handler Token
login username = liftIO $ do
    --print $ getUsers username
    return token11



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


type API = "login" :> QueryParam "username" String :> Get '[JSON] Token


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server


api :: Proxy API
api = Proxy

server :: Server API
server = login


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

-- printData = runMongo allCollections

-- findFirstFile = runMongo $ findOne $ select [] "files"

-- findAllFiles = runMongo $ find (select [] "files") >>= rest

insertFile :: Document -> IO ()
insertFile inFile = runMongo $ insert "files" inFile

insertUser :: Document -> IO ()
insertUser inUser = runMongo $ insert "users" inUser

deleteFile :: Document -> IO ()
deleteFile delFile = runMongo $ delete $ select delFile "files"

saveFile :: TheFile -> Handler ResponseData
saveFile theFile = liftIO $ do
    --print(theFile)
    let fc = theContents theFile
    let fileToSave = caesarEncrypt (key privateKey) fc

    let encryptedFile = TheFile fileToSave
    --print(encryptedFile)
    e <- insertFile ( toBSON encryptedFile)
    return $ ResponseData (theContents encryptedFile)


addUser :: User -> Handler ResponseData
addUser theUser = liftIO $ do
    let theName = username theUser
    let thePassword = password theUser
    let encryptedPassword = caesarEncrypt (key privateKey) thePassword

    let userToAdd = User theName encryptedPassword
    e <- insertUser ( toBSON userToAdd)
    return $ ResponseData (username userToAdd)


getUsers :: Maybe String -> Handler [User]
getUsers username = liftIO $ do
    listOfUsers <- returnMongo $ find (select ["username" =: username] "users") >>= rest
    return $ mapMaybe (\ b -> fromBSON b :: Maybe User) listOfUsers




users :: [User]
users = [ User "clinced" "p@ssw0rd"
        , User "saint_nick" "12345"
        ]

--tests encrypting and decrypting a file
main = do
    theFile <- readFile "text.txt"
    putStrLn ("Original file: " ++ theFile)
    theKey  <- generateKey
    --test
    let temp = caesarEncrypt theKey theFile
    putStrLn ("Encrypted file: " ++ temp)
    putStrLn ""

    let temp2 = caesarDecrypt theKey temp
    putStrLn ("Decrypted file: " ++ temp2)


--curl -X POST -d '{"file": "123"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/saveFile

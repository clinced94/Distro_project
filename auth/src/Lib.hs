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
import           Data.Char
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing)
import           Data.String
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
privateKey = Key 7

token1 :: Token
token1 = Token 11


encrypt :: String -> Int -> String
encrypt theFile theKey = do
    let theFileInt = map ord theFile
    let applyTheKey = map (+theKey) theFileInt
    let encryptedMsg = map chr applyTheKey
    return encryptedMsg!!0 --return first element

decrypt :: String -> Int -> String
decrypt theFiletoDecrypt theDecryptionKey = do
    let decryptFileInt = map ord theFiletoDecrypt
    let applyTheKey = map (+(-theDecryptionKey)) decryptFileInt
    let decryptedMsg = map chr applyTheKey
    return decryptedMsg!!0



--database interaction
data User = User
  { userId   :: Int
  , username :: String
  , password :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data TheFile = TheFile
    { theContents :: String
    } deriving (Show, Read, Generic, FromJSON, ToJSON, FromBSON, ToBSON)

deriving instance FromBSON String
deriving instance ToBSON String

data ResponseData = ResponseData
    { response :: String
    } deriving Generic

instance ToJSON ResponseData
instance FromJSON ResponseData


type API = "users" :> Get '[JSON] [User]
            :<|> "token1" :> Get '[JSON] Token
            :<|> "saveFile" :> ReqBody '[JSON] TheFile :> Post '[JSON] ResponseData


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server


api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> return token1
    :<|> saveFile


runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "fileCabinet" functionToRun
    print e
    close pipe

printData = runMongo allCollections

findFirstFile = runMongo $ findOne $ select [] "files"

findAllFiles = runMongo $ find (select [] "files") >>= rest

insertFile :: Document -> IO()
insertFile inFile = runMongo $ insert "files" inFile

deleteFile :: Document -> IO()
deleteFile delFile = runMongo $ delete $ select delFile "files"

saveFile :: TheFile -> Handler ResponseData
saveFile theFile = liftIO $ do
    print(theFile)
    let fc = theContents theFile
    let fileToSave = encrypt fc (key privateKey)

    let encryptedFile = TheFile fileToSave
    print(encryptedFile)
    e <- insertFile $ (toBSON $ encryptedFile)
    return $ ResponseData (theContents encryptedFile)

--TODO: API to post users to db

--TODO: getUsers



users :: [User]
users = [ User 1 "clinced" "p@ssw0rd"
        , User 2 "saint_nick" "12345"
        ]

{- tests encrypting and decrypting a file
main = do
    handle  <- openFile "text.txt" ReadMode
    theFile <- hGetContents handle
    theKey  <- generateKey
    --test
    let temp = encrypt theFile theKey
    putStrLn (temp)
    let temp2 = decrypt temp theKey
    putStrLn (temp2) -}


--curl -X POST -d '{"file": "123"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/saveFile

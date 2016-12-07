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
import           System.Random

data Key = Key
    {key :: Int
    } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Token = Token
    { token :: Int
    } deriving (Show, Read, FromJSON, ToJSON)


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8080."
    run 8080 app

app :: Application
app = serve api server

type API = "login" :> QueryParam "username" String :> Get '[JSON] Token
        :<|> "publicKey" :> Get '[JSON] Key

api :: Proxy API
api = Proxy

server :: Server API
server = login
    :<|> return publicKey

--TODO: login function


generateKey :: IO Int
generateKey = randmRIO(1,25)

--test Keys
publicKey :: Key
publicKey = 13

privateKey :: Key
privateKey = 7


encrypt :: String -> Int -> String
encrypt theFile theKey = do
    let theFileInt = map ord theFile
    let applyTheKey = map (+theKey) theFileInt
    let encryptedMsg = map chr applyTheKey
    return encryptedMsg!!0 --return first element

decrypt :: String -> Int -> String
decrypt theFiletoDecrypt theDecryptionKey = do
    let decryptFileInt = map ord theFiletoDecrypt
    let applyTheKey = map (+(-theDecryptionKey) decryptFileInt
    let decryptedMsg = map chr applyTheKey
    return decryptedMsg!!0
    

data User = User
  { username       :: String
  , password       :: String
  } deriving (Eq, Show, FromJSON, ToJSON)

data UserFile = UserFile
    { file :: String
    } deriving (Show, Generic, FromJSON, ToJSON)

data ResponseData = ResponseData
    { response :: String
    } deriving Generic

instance ToJSON ResponseData
instance FromJSON ResponseData


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

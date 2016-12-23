{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Lib
    ( startApp
    ) where

import           Control.Monad            (forever, when)
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy
import           Data.String
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.Exit


startApp :: IO ()
startApp = forever $ do
    l <- getLine
    when (l == "login") $ do
        putStrLn ""
        putStrLn "Enter username: "
    when (l == "logout") $ do
        putStrLn ""
        putStrLn "Goodbye"
        exitSuccess


data Token = Token
    { token :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)

data PublicKey = Key
    {theKey :: Int
    } deriving (Show, Generic, FromJSON, ToJSON)

getPublicKey :: ClientM PublicKey


login :: Maybe String -> ClientM Token

type AuthAPI = "login" :> QueryParam "username" String :> Get '[JSON] Token
        :<|> "getPublicKey" :> Get '[JSON] PublicKey

authAPI :: Proxy AuthAPI
authAPI = Proxy

(login :<|> getPublicKey) = client authAPI

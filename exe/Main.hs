--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the main entry point to the program and implements
-- the web server.
module Main ( main ) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson

import qualified Network.HTTP.Media as M
import Network.Wai.Handler.Warp (withApplication)

import Text.Printf

import WaiAppStatic.Types
import WaiAppStatic.Storage.Filesystem

import Servant

import Convert
import Interpreter

--------------------------------------------------------------------------------

-- | A type representing XML content.
data XML

-- | We can accept XML as content.
instance Accept XML where
    contentType _ = "text" M.// "xml"

-- | Convert Google Blockly XML.
instance MimeUnrender XML Doc where
    mimeUnrender _ = convert

--------------------------------------------------------------------------------

-- | Represents possible outcomes of running a program.
data RunResponse
    = RunSuccess Memory
    | RunFailure String

-- | Converts responses to JSON.
instance ToJSON RunResponse where
    toJSON (RunSuccess mem) = object ["memory" .= toJSON mem]
    toJSON (RunFailure err) = object ["error"  .= err]

--------------------------------------------------------------------------------

-- | The web application's API as a type.
type ScratchAPI = "run" :> ReqBody '[XML] Doc :> Post '[JSON] RunResponse
             :<|> Raw

-- | A proxy value for the web application's API.
scratchAPI :: Proxy ScratchAPI
scratchAPI = Proxy

-- | The web server's configuration.
webAppSettings :: StaticSettings
webAppSettings = (defaultWebAppSettings ".") {
        ssRedirectToIndex = True,
        ssIndices = map unsafeToPiece ["index.html"]
    }

-- | The request handler for evaluation requests.
runInterpreter :: Doc -> Handler RunResponse
runInterpreter (Doc vs stmts) =
    case interpret stmts [(v,0) | v <- vs] of
        Left err -> return $ RunFailure (show err)
        Right mem -> return $ RunSuccess mem

-- | The web server's request handler.
scratchServer :: Server ScratchAPI
scratchServer = runInterpreter
           :<|> serveDirectoryWith webAppSettings

-- | The web application.
scratchApp :: Application
scratchApp = serve scratchAPI scratchServer

-- | The main entry point for this application which starts the web server.
main :: IO ()
main = do
    putStrLn "Starting web server..."
    withApplication (pure scratchApp) $ \port -> do 
        putStrLn $ printf "Started on http://localhost:%d" port
        putStrLn "Press enter to quit."
        void getChar

--------------------------------------------------------------------------------

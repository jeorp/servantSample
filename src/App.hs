{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module App where

import Data.Aeson
import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import qualified Data.ByteString.Lazy as B
import Control.Arrow 
import Control.Lens ((^.), to)
import Control.Monad.Reader
import Database.SQLite.Simple
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Lucid
import Network.HTTP.Media ((//), (/:))
import qualified Network.Wai.Handler.Warp as Warp

import Todo
import Model
import Common
import CrudSqlite

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTMLLucid B.ByteString where
    mimeRender _ = id

instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS

type API = Get '[HTMLLucid] B.ByteString
         :<|> "static" :> Raw
         :<|> CRUD


api :: Proxy API
api = Proxy

readerServer :: ServerT API (ReaderT Config Handler)
readerServer = home
       :<|> serveDirectoryWebApp "static"
       :<|> getToDoAll
       :<|> postTodo
       :<|> putTodoId
       :<|> deleteTodoId
  where
    home = liftIO $ B.readFile "templates/index.html"
    getToDoAll = do
        config <- ask
        selectData (Query (T.pack ("select * from " <> config ^. table <> ";") )) (config ^. dbPath) []
    postTodo todo = do
        config <- ask
        liftIO $ print todo
        addTodo (config ^. dbPath) (config ^. table) ((^. title) &&& (^. done . to intToByte) $ todo) []
    putTodoId i todo = do
        config <- ask
        updateTodo (config ^. dbPath) (config ^. table) i ((^. title) &&& (^. done . to intToByte) $ todo) []
    deleteTodoId i = do
        config <- ask
        liftIO $ print i
        deleteTodo (config ^. dbPath) (config ^. table) i []
    intToByte :: Int -> Int
    intToByte n = if n > 0 then 1 else 0

start :: IO ()
start = do
  putStrLn "Listening on port 8080"
  let server = hoistServer api (`runReaderT` defaultConfig) readerServer
  Warp.run 8080 $ serve api server

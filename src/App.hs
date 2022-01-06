{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module App where

import Data.Aeson
import Control.Monad.IO.Class
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T 
import qualified Data.ByteString.Lazy as B
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Lucid
import Network.HTTP.Media ((//), (/:))
import qualified Network.Wai.Handler.Warp as Warp

import Todo
import Model

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

server :: Server API
server = home
       :<|> serveDirectoryWebApp "static"
       :<|> getToDoAll
       :<|> postTodo
       :<|> putTodoId
       :<|> deleteTodoId
  where
    home = liftIO $ B.readFile "templates/index.html"
    getToDoAll = pure [Todo 1 "sample" False ""]
    postTodo todo = pure todo
    putTodoId id todo = pure ()
    deleteTodoId id = pure ()

start :: IO ()
start = do
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve api server

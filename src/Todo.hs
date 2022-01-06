{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo where

import Data.Aeson
import Data.Proxy
import qualified Data.Text as Text

import Servant.API
import Web.FormUrlEncoded (FromForm(..), parseUnique)
import Model


instance FromForm Todo where
  fromForm form = Todo
              <$> parseUnique "todoId" form
              <*> parseUnique "title" form
              <*> parseUnique "done" form
              <*> parseUnique "created_at" form

type CRUD =    "todo" :> "all" :> Get '[JSON] [Todo]
          :<|> "todo" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] ()
          :<|> "todo" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] ()
          :<|> "todo" :> Capture "id" Int :> Delete '[JSON] ()

crud :: Proxy CRUD
crud = Proxy
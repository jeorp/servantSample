{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import GHC.Generics
import Data.Aeson
import Control.Lens


data Todo = Todo
  { _todoId :: Integer
  , _title  :: String
  , _done   :: Bool
  , _created_at :: String 
  } deriving (Generic, FromJSON, ToJSON)

makeLenses ''Todo

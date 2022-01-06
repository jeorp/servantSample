{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import GHC.Generics
import Data.Aeson
import Control.Lens
import Data.Time.LocalTime


data Todo = Todo
  { _todoId :: Integer
  , _title  :: String
  , _done   :: Int
  , _created_at :: String 
  } deriving (Generic, FromJSON, ToJSON, Show, Eq)

makeLenses ''Todo

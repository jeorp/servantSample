{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model where

import GHC.Generics
import Data.Aeson

data Todo = Todo
  { todoId :: Int
  , title  :: String
  , done   :: Bool
  } deriving (Generic, FromJSON, ToJSON)
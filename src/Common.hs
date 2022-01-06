{-# LANGUAGE TemplateHaskell #-}
module Common where

import Control.Lens

data Config = Config 
  {
      _dbPath :: String,
      _table :: String
  }deriving (Show, Eq)

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config "todo.db" "todo"
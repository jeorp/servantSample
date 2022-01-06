{-# LANGUAGE OverloadedStrings #-}
module CrudSqlite where

import qualified Data.Text as T
import System.Directory 
import Control.Exception.Safe
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow 
import Model

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field

instance ToRow Todo where
  toRow (Todo i t d c) = toRow (i, t ,d, c)

todoMigrateQuery :: String -> Query 
todoMigrateQuery table = 
  "create table if not exists " <> 
  Query (T.pack table) <> 
  " (id INTEGER PRIMARY KEY, title TEXT NOT NULL, done INTEGER NOT NULL, created_at TIMESTAMP DEFAULT (datetime(CURRENT_TIMESTAMP,'localtime')) )"

openDatabase :: (MonadIO m, MonadCatch m) => m Connection -> (Connection -> m a ) -> m a
openDatabase ioConnect connectIO = ioConnect >>= (\conn -> connectIO conn <* liftIO (close conn))


migrateModel :: (MonadIO m, MonadCatch m) => Query -> String -> (SQLError -> m ()) -> m ()
migrateModel query path errorHandle = do
    openDatabase (liftIO $ open path) $ \conn ->
      liftIO (execute_ conn query) `catch` errorHandle

flushTableData :: (MonadIO m, MonadCatch m) => String -> String -> (SQLError -> m ()) -> m ()
flushTableData table_name path errorHandler = do
    openDatabase (liftIO $ open path) $ \conn ->
      liftIO (execute_ conn ("delete from " <> Query (T.pack table_name) <> " ;")) `catch` errorHandler

--select example
selectData :: (FromRow q, MonadIO m, MonadCatch m) => Query -> String -> [Handler m [q]] -> m [q]
selectData query path errorHnadle = do
    -- does File exists then action else return ()
    bool <- liftIO $ doesFileExist path
    if bool 
      then openDatabase (liftIO $ open path) $ 
        \c -> liftIO (query_ c query )`catches` errorHnadle
      else return []

addTodo :: (MonadIO m, MonadCatch m) => String -> String -> (String, Int) -> [Handler m ()] -> m ()
addTodo path table (t, d) errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (execute conn ("insert into " <> Query (T.pack table) <> " (title, done) values (?, ?)") (t, d))
      `catches` 
      errorHandl
    
updateDone :: (MonadIO m, MonadCatch m) => String -> String -> Integer -> Int -> [Handler m ()] -> m ()
updateDone path table i b errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (executeNamed conn ("UPDATE " <> Query (T.pack table) <> " SET done = :done WHERE id = :id") [":done" := (b :: Int), ":id" := i])
      `catches` 
      errorHandl

updateTitle :: (MonadIO m, MonadCatch m) => String -> String -> Integer -> String -> [Handler m ()] -> m ()
updateTitle path table i t errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (executeNamed conn ("UPDATE " <> Query (T.pack table) <> " SET title = :title WHERE id = :id") [":title" := (t :: String), ":id" := i])
      `catches` 
      errorHandl

updateTodo :: (MonadIO m, MonadCatch m) => String -> String -> Integer -> (String, Int) -> [Handler m ()] -> m ()
updateTodo path table i (t,b) errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (executeNamed conn ("UPDATE " <> Query (T.pack table) <> " SET title = :title done = :done WHERE id = :id") [":title" := (t :: String), ":done" := (b :: Int), ":id" := i])
      `catches` 
      errorHandl

deleteTodo :: (MonadIO m, MonadCatch m) => String -> String -> Integer -> [Handler m ()] -> m ()
deleteTodo path table i errorHandl = do
    openDatabase (liftIO $ open path) $ \conn -> 
      liftIO (liftIO (execute conn ("DELETE FROM " <> Query (T.pack table) <>  " WHERE id = ?") (Only i)))
      `catches` 
      errorHandl
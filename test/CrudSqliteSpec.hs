{-# LANGUAGE OverloadedStrings  #-}
module CrudSqliteSpec where

import Test.Hspec
import Control.Lens
import System.Directory
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Exception.Safe
import Database.SQLite.Simple
import CrudSqlite
import Model

main = hspec spec

db = "test.db"

table = "test_todo"

migrateTest :: IO ()
migrateTest = migrateModel (todoMigrateQuery table) db (liftIO . print)

addTest :: (String, Int) -> IO ()
addTest todo = addTodo db table todo []

updateDoneTest :: Integer -> Int -> IO ()
updateDoneTest i b = updateDone db table i b []

updateTitleTest :: Integer -> String -> IO ()
updateTitleTest i t = updateTitle db table i t []

updateTodoTest :: Integer -> (String, Int) -> IO ()
updateTodoTest i td = updateTodo db table i td []

deleteTodoTest :: Integer -> IO ()
deleteTodoTest i = deleteTodo db table i []

selectDataTest :: Query -> IO [Todo]
selectDataTest q = selectData q db []


testSample1 :: IO Bool
testSample1 = do
  migrateTest
  addTest ("sample1", 0)
  xs <- selectDataTest ("select * from " <> Query (T.pack table) <> " where title = 'sample1';") :: IO [Todo]
  pure (head xs ^. title == "sample1") <* removeFile db

testSample2 :: IO Int
testSample2 = do
  migrateTest
  addTest ("sample2", 0)
  xs <- selectDataTest ("select * from " <> Query (T.pack table) <> " where title = 'sample2';") :: IO [Todo]
  updateDoneTest (head xs ^. todoId) 1
  ys <- selectDataTest ("select * from " <> Query (T.pack table) <> " where title = 'sample2';") :: IO [Todo]
  pure (head ys ^. done) <* removeFile db

spec :: Spec
spec = do
  describe "Test Crud sqlite" $ do
    it "Test sample 1" $ do
      testSample1 `shouldReturn` True
    it "Test sample 2" $ do
      testSample2 `shouldReturn` 1
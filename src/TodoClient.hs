{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module TodoClient where

import Control.Monad (void)
import Control.Monad.IO.Class
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Todo
import Model

getTodoAll   :: ClientM [Todo]
postTodo     :: Todo -> ClientM ()
putTodoId    :: Int -> Todo -> ClientM ()
deleteTodoId :: Int -> ClientM ()

getTodoAll :<|> postTodo :<|> putTodoId :<|> deleteTodoId = client crud
 

executeClient :: ClientM a -> IO (Either ClientError a)
executeClient cli =  do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "localhost" 8080 ""
  runClientM cli env
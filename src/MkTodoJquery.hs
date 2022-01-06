module MkTodoJquery where
import Servant.JS
import Todo

mkTodoJquery :: IO ()
mkTodoJquery = writeJSForAPI crud jquery "static/todo_crud.js"
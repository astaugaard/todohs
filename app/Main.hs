{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory, getXdgDirectory, XdgDirectory (XdgData))

data Commands = Add String | Complete Int | List | Done | Todo | Archive | Archived | MakeEww String String Int

data Todos = Todos {done :: [String], todo :: [String]} deriving (Show, Generic)

instance ToJSON Todos

instance FromJSON Todos

parseCommand :: O.Parser Commands
parseCommand =
  O.subparser $
    O.command "add" (O.info (Add <$> O.argument O.str (O.metavar "TODO")) (O.progDesc "add TODO - add TODO to todo list"))
      <> O.command "complete" (O.info (Complete <$> O.argument O.auto (O.metavar "ITEM")) (O.progDesc "complete ITEM - complete item number ITEM on the todo list"))
      <> O.command "list" (O.info (pure List) (O.progDesc "list all items and done items on todo list todo first"))
      <> O.command "done" (O.info (pure Done) (O.progDesc "list all done items on todo list"))
      <> O.command "archive" (O.info (pure Archive) (O.progDesc "archive all done items"))
      <> O.command "todo" (O.info (pure Todo) (O.progDesc "list all items todo on the todo list"))
      <> O.command "archived" (O.info (pure Archived) (O.progDesc "list all archived todos"))
      <> O.command "generateEww" (O.info (MakeEww <$> O.argument O.str (O.metavar "VAR") <*> O.argument O.str (O.metavar "COM") <*> O.argument O.auto (O.metavar "NUM")) (O.progDesc "generateEww VAR COM -- ouput YUCK to to make eww todo list showing NUM todos that executes COM to update VAR literal widget"))

store :: IO FilePath
store = getXdgDirectory XdgData "todohs"

decodeData :: FromJSON a => FilePath -> IO a
decodeData f = do
  s <- store
  json <- decode <$> B.readFile (s ++ "/" ++ f)
  case json of
    Just a -> return a
    Nothing -> error "file invalid format"

saveData :: ToJSON a => String -> a -> IO ()
saveData f d = do
  s <- store
  B.writeFile (s ++ "/" ++ f) $ encode d

getTodos :: IO Todos
getTodos = decodeData "todos"

saveTodos :: Todos -> IO ()
saveTodos = saveData "todos"

getArchive :: IO [String]
getArchive = decodeData "done"

saveArchive :: [String] -> IO ()
saveArchive = saveData "done"

showTodo :: Todos -> String
showTodo = concat . zipWith (\x y -> show x ++ ") " ++ y ++ "\n") ([1 ..] :: [Int]) . todo

oneButton :: String -> String -> Int -> Int -> String -> String
oneButton v c a i n = "(button :timeout \"2s\" :onclick " ++ show updateCom ++ " " ++ show n ++ " )"
    where generateEwwCom = "`todohs generateEww " ++ v ++ " " ++ show c ++ " " ++ show a ++ "`"
          updateCom = "todohs complete " ++ show i ++ " && " ++ c ++ " update " ++ v ++ "=" ++ show generateEwwCom


makeEww :: String -> String -> Int -> Todos -> String
makeEww v c i todos = "(box :orientation \"v\" :class \"todoList\" " ++ concat t ++ ")"
    where t = zipWith (oneButton v c i) [1..] $ take i $ todo todos

run :: Commands -> IO ()
run (MakeEww v c i) = do todos <- getTodos
                         putStrLn $ makeEww v c i todos
run (Add s) = do
  todos <- getTodos
  saveTodos $ todos {todo = s : todo todos}
run (Complete n) = do
  todos <- getTodos
  when (length (todo todos) < n) $ error "invalid index"
  saveTodos $ todos {todo = take (n - 1) (todo todos) ++ drop n (todo todos), done = (todo todos !! (n - 1)) : done todos}
run List = do
  todos <- getTodos
  putStrLn "TODO: "
  putStr $ showTodo todos
  putStrLn "DONE: "
  mapM_ putStrLn $ done todos
run Done = do
  todos <- getTodos
  mapM_ putStrLn $ done todos
run Todo = do
  todos <- getTodos
  putStr $ showTodo todos
run Archive = do
  a <- getArchive
  t <- getTodos
  saveArchive $ done t ++ a
  saveTodos $ t {done = []}
run Archived = do
  a <- getArchive
  mapM_ putStrLn a



main :: IO ()
main = do
  setupFileSystem
  O.execParser opts >>= run
  where
    opts = O.info (parseCommand O.<**> O.helper) (O.fullDesc <> O.header "todohs - a todo list management cli app written in haskell")

setupFileSystem :: IO ()
setupFileSystem = do
  h <- store
  createDirectoryIfMissing True h
  b <- doesFileExist $ h ++ "/todos"
  if b
    then return ()
    else B.writeFile (h ++ "/todos") $ encode $ Todos [] []
  b' <- doesFileExist (h ++ "/done")
  if b'
    then return ()
    else B.writeFile (h ++ "/done") $ encode ([] :: [String])

-- {-# LANGUAGE NoMonomorphismRestriction #-}
module DbEg where

import Steshaw ((|>))
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)
import Control.Monad.Reader

type ImplicitConnection = Reader (Connection)

connect = connectSqlite3 "test1.db"

withConnection f = do
  c <- connect
  result <- f c
  disconnect c
  return result

populate =
  withConnection $ \c -> do
    tables <- getTables c
    when (notElem "descs" tables) $ do
      run c "create table descs (id integer not null, desc varchar(200) not null)" []
      insert <- prepare c "insert into descs (id, desc) values (?, ?)"
      mapM (execute insert)
        [ [toSql (1::Integer), toSql "one"]
        , [toSql (2::Integer), toSql "two"]
        , [toSql (3::Integer), toSql "three"]
        , [toSql (4::Integer), toSql "four"]
        , [toSql (5::Integer), toSql "five"]
        , [toSql (6::Integer), toSql "six"]
        ]
      commit c

--

allSql = ("select * from descs", [])

descSql = ("select desc from descs", [])

searchSql s = ("select desc from descs where desc like ?", [s])

--

dumpAll = withConnection $ \c ->
  quickQuery' c `uncurry` allSql

dumpDesc = withConnection $ \c ->
  quickQuery' c `uncurry` descSql

search s = withConnection $ \c ->
  quickQuery' c `uncurry` (searchSql s)

--

dumpAllI :: ImplicitConnection (IO [[SqlValue]])
dumpAllI = do 
  c <- ask
  return $ quickQuery' c `uncurry` allSql

dumpDescI :: ImplicitConnection (IO [[SqlValue]])
dumpDescI = do
  c <- ask
  return $ quickQuery' c `uncurry` descSql

searchI :: SqlValue -> ImplicitConnection (IO [[SqlValue]])
searchI s = do 
  c <- ask
  return $ quickQuery' c `uncurry` (searchSql s)

--

runDumpAllI = withConnection $ \c -> runReader dumpAllI c

runDumpDescI = withConnection $ \c -> runReader dumpDescI c

runSearchI s = withConnection $ \c -> runReader (searchI (toSql s)) c

--

goDumpAll  = runDumpAllI  >>= mapM_ print

goDumpDesc = runDumpDescI >>= mapM_ print

goSearch s = runSearchI s >>= mapM_ print

--

eg1 = withConnection $ \c -> do
  putStrLn "all:"
  runReader dumpAllI c >>= mapM_ print
  putStrLn "\ndesc:"
  runReader dumpDescI c >>= mapM_ print
  forM_ ["foo", "two", "f%"] $ \s -> do
    putStrLn $ "\nsearch " ++ show s ++ ":"
    runReader (searchI (toSql s)) c >>= mapM_ print

module DbEg where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)

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

search s = withConnection $ \c ->
  quickQuery' c "select desc from descs where desc like ?" [s]

allDesc = withConnection $ \c ->
  quickQuery' c "select desc from descs" []

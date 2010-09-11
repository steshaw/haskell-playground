module DbEg where

import Database.HDBC
import Database.HDBC.Sqlite3

connect = connectSqlite3 "test1.db"

populate = do
  c <- connect
  tables <- getTables c
  if notElem "descs" tables
    then do
      run c "create table descs (id integer not null, desc varchar(200) not null)" []
      insert <- prepare c "insert into descs (id, desc) values (?, ?)"
      mapM (execute insert) [
        [toSql (1::Integer), toSql "one"],
        [toSql (2::Integer), toSql "two"],
        [toSql (3::Integer), toSql "three"],
        [toSql (4::Integer), toSql "four"],
        [toSql (5::Integer), toSql "five"],
        [toSql (6::Integer), toSql "six"]
       ]
      commit c
    else return ()
  disconnect c

search s = do
  c <- connect
  result <- quickQuery' c "select desc from descs where desc like ?" [s]
  disconnect c
  return result

allDesc = do
  c <- connect
  result <- quickQuery' c "select desc from descs" []
  disconnect c
  return result

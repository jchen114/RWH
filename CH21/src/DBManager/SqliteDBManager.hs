module SqliteDBManager (
 DBConnection (..)
 , DBInteger (..)
 , DBText (..)
 , DBFieldType
 , DBText
 , createDBConnection
 , createTable
 , saveToDB
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Arrow
import Data.List

data DBConnection = DBConnection {
 DBFile :: String
}

data DBInteger = INTEGER | INTEGER_PK | INTEGER_PKAI
data DBText = TEXT | VARCHAR Integer

data DBFieldType = DBFInteger DBInteger | DBFText DBText

data DBTable = DBTable {
 name :: String
 params :: [(String, DBFieldType)]
} 

dBIntegerToString :: DBInteger -> String
dBIntegerToString dbi = case dbi of
 INTEGER -> "INTEGER"
 INTEGER_PK -> "INTEGER PRIMARY KEY"
 INTEGER_PKAI -> "INTEGER PRIMARY KEY AUTO INCREMENT"

dBTextToString :: DBText -> String
dBTextToString dbt = case dbt of
 TEXT -> "TEXT"
 VARCHAR i -> "VARCHAR(" ++ (show i) ++ ")"

dBFieldTypeToString -> DBFieldType -> String
dBFiledTypeToString dbf = case dbf of
 DBFInteger dbi -> dBIntegerToString dbi
 DBFText dbt -> dBTextToString dbt

createDBConnection :: DBConnection -> IO Connection
createDBConnection (DBConnection DBFile) = do
 db <- catchSql (connectSqlite3 DBFile) handle
  where handle err
 
createTable :: (IConnection a) -> DBTable -> IO () 
createTable conn (DBTable name params) = do 
 run conn ("CREATE TABLE" ++ name ++ "(" ++ (params2Str params) ++ ");") []
 commit db

params2Str :: [(String, DBFieldType)] ->String
params2Str params = intercalate "," $ map tup2Str $ map (second dBFieldTypeToString) 
 where tup2Str a b = a ++ ", " ++ b

saveToDB :: (IConnection d, Convertible a SqlValue, MonadIO m) => d -> String -> [(String, a)] -> m Integer
saveToDB db tblName cv = do
 let query = "INSERT INTO " ++ tblName ++ "(" ++ (columns cv) ++ ")" ++ " VALUES" ++ "(" ++ (createQs cv) ++ ")"
 let vals = valToSql cv
 liftIO $ withTransaction db $ \d -> run d query vals
 [[uid]] <- liftIO $ quickQuery db "select last_insert_row_id()" []
 return (fromSql uid)
  where createQs cv = init $ replicate (length cv) "?,"
        columns = intercalate "," $ (fst . unzip)
        valToSql cv = fmap toSql $ snd unzip cv
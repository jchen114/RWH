module DBManager.PostGreSqlDBManager ( DBConnection (..)
                 , DBInteger (..)
                 , DBTable (..)
                 , DBText (..)
                 , DBFieldType (..)
                 , connectionString
                 , createTable
                 ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Control.Arrow
import Data.List

data DBConnection = DBConnection {
 host :: String,
 port :: String,
 dbName :: String,
 user :: String,
 password :: String
} deriving (Show)

data DBInteger = IntegerPrimaryKey | IntegerNotNull | Integer
data DBText = TextNotNull | Text

data DBFieldType = DBFInteger DBInteger | DBFText DBText

data DBTable = DBTable {
 name :: String,
 params :: [(String, DBFieldType)]
}

dBIntegerToString :: DBInteger -> String
dBIntegerToString t = case t of
 IntegerPrimaryKey -> "INT PRIMARY KEY NOT NULL"
 IntegerNotNull -> "INT NOT NULL"
 Integer -> "INT"

dBTextToString :: DBText -> String
dBTextToString t = case t of
 TextNotNull -> "TEXT NOT NULL"
 Text -> "TEXT"

dBFieldTypeToString :: DBFieldType -> String
dBFieldTypeToString ft = case ft of 
 DBFInteger i -> dBIntegerToString i
 DBFText t -> dBTextToString t

-- (host, port, database name, user, password)
connectionString :: DBConnection -> String
connectionString (DBConnection host port dbName user password) = "host=" ++ host ++ " port=" ++ port ++ " dbname=" ++ dbName ++ " user=" ++ user ++ " password=" ++ password

createTable ::  DBTable -> String
createTable (DBTable name params) = "CREATE TABLE " ++ name ++ " (" ++ (paramsToString params) ++ ");"
 where paramsToString params = intercalate ", " $ map tupleToString $ map (second dBFieldTypeToString) params

tupleToString :: (String, String) -> String
tupleToString (a,b) = a ++ " " ++ b

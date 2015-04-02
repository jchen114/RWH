import Database.HDBC
import Database.HDBC.PostgreSQL
import DBManager

main :: IO()
main = do
 conn <- connectPostgreSQL $ connectionString (DBConnection "localhost" "5432" "Kingpin" "Kingpin" "password")
 putStrLn $ createTable (DBTable "test" [("ID", DBFInteger IntegerPrimaryKey), ("NAME", DBFText Text)])
 --run conn "CREATE TABLE test (ID INT PRIMARY KEY NOT NULL, NAME TEXT NOT NULL);" []
 --run conn "INSERT INTO test (ID, NAME) VALUES (0, 'JIMMY');" []
 --commit conn
 disconnect conn

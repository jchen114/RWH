import PostGreSqlDBManager

main :: IO ()
main = do
 conn <- connectPostgreSQL $ connectionString (DBConnection "localhost" "5432" "Kingpin" "Kingpin" "password")
 --putStrLn $ createTable (DBTable "test" [("ID", DBFInteger IntegerPrimaryKey), ("NAME", DBFText Text)])
 run conn "CREATE TABLE test01 (ID INT PRIMARY KEY NOT NULL, NAME TEXT NOT NULL);" []
 --run conn "INSERT INTO test (ID, NAME) VALUES (0, 'JIMMY');" []
 --commit conn
 disconnect conn

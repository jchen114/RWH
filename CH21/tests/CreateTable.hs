import DBManager.SqliteDBManager

main :: IO ()
main = do
 conn <- createDBConnection $ DBConnection "src/testdb"
 case conn of Left str -> putStrLn "fail"
              Right c  -> testDB c
  where testDB c = createTable c $ DBTable "testTable" [("name", DBFText TEXT)]

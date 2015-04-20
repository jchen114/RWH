import DBManager.SqliteDBManager

main :: IO ()
main = do
 conn <- createDBConnection $ DBConnection "src/testdb"
 case conn of Left str -> putStrLn str
              Right c  -> testDB c
  where testDB c = createTable c $ DBTable "testTable2" [("name", DBFText TEXT)]

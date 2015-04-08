import DBManager.SqlLiteDBManager

main :: IO ()
main = do
 conn <- createDBConnection
 disconnect conn



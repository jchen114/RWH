import SqlLiteDBManager

main :: IO ()
main = do
 conn <- createDBConnection
 disconnect conn



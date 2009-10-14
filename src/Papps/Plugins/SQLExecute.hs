module Papps.Plugins.SQLExecute (
    sqlResults
  )
  where

#ifdef sql-execute
import Database.HDBC
import Database.HDBC.MySQL

import Data.List (intercalate)

sqlResults :: String -> IO String
sqlResults sql = do
  conn <- connectMySQL $ defaultMySQLConnectInfo {
                            mysqlHost = "localhost"
                          , mysqlUser = "moverman"
                          , mysqlPassword = ""
                          , mysqlDatabase = ""
                          , mysqlPort = 3306
                          , mysqlUnixSocket = ""}
  stmt <- prepare conn sql  
  execute stmt []
  res <- fetchAllRows stmt 
  colNames <- getColumnNames stmt
  return $ 
    formatSql colNames res

formatSql :: [String] -> [[SqlValue]] -> String
formatSql _    [[]] = "Query yields zero rows."
formatSql cols res  = do
    let r = map (map f)  res
    []
  where f (SqlString  s) = s
        f (SqlInteger s) = show s
        f (SqlDouble  s) = show s
        f (SqlBool    s) = show s
        f (SqlNull     ) = ""
        f t              = show t 

#else

sqlResults :: String -> IO String
sqlResults sql = ""

#endif

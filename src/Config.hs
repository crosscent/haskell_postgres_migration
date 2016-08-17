module Config 
    ( getConf
    , genPostgreSQLConf
    , ConfPostgreSQL (..)
    )
   where

import System.Environment   ( getArgs
                            , lookupEnv)
import Data.ByteString              ( ByteString(..) )
import Data.ByteString.Char8        ( pack )
import Data.List                    ( intercalate )
import Data.Maybe           ( fromMaybe )

data ConfPostgreSQL = ConfPostgreSQL
    { getUsername   :: Maybe String
    , getPassword   :: Maybe String
    , getHost     :: Maybe String
    , getDatabase :: Maybe String
    }

getConf' :: String -> [String] -> Maybe String
getConf' var [] = Nothing
getConf' var (arg:args)
  | var == take (length var) arg    = Just $ tail s
  | otherwise     = getConf' var args
    where (w, s) = break (=='=') arg

getConf :: String -> IO (Maybe String)
getConf var = do
    arg <- fmap (getConf' ("--" ++ var)) getArgs
    if arg == Nothing
       then lookupEnv var
       else return arg

genPostgreSQLConf' :: String -> Maybe String -> String
genPostgreSQLConf' _ Nothing = ""
genPostgreSQLConf' header (Just x) = header ++ x

genPostgreSQLConf :: ConfPostgreSQL -> ByteString
genPostgreSQLConf x = pack $ intercalate " " $ filter (\x -> length x > 0) $ [ username, password, host, database ]
    where username = genPostgreSQLConf' "user=" $ getUsername x
          password = genPostgreSQLConf' "password=" $ getPassword x
          host = genPostgreSQLConf' "host=" $ getHost x
          database = genPostgreSQLConf' "database=" $ getDatabase x

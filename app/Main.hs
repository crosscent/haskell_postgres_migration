{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Char8        ( pack )

import Database.PostgreSQL.Simple   ( close
                                    , connectPostgreSQL
                                    , execute_
                                    , query_
                                    , Connection
                                    , Only (..)
                                    , Query (..) )

import Config                       ( genPostgreSQLConf
                                    , getConf
                                    , ConfPostgreSQL (..))

import MigrationFile                ( migrationFiles )

isInitial :: Connection -> IO Bool
isInitial conn = do
    [Only i] <- query_ conn "SELECT count(*) FROM information_schema.tables WHERE table_name = 'haskell_postgres_migration';"
    return $ i == (0 :: Integer)

lastMigration :: Connection -> IO Integer
lastMigration conn = do
    [Only i] <- query_ conn "SELECT version FROM haskell_postgres_migration order by timestamp desc limit 1;"
    return i


createMigrationSchema :: Connection -> IO ()
createMigrationSchema conn = do
    execute_ conn "CREATE TABLE haskell_postgres_migration(ID SERIAL PRIMARY KEY,VERSION INT NOT NULL, TIMESTAMP TIMESTAMP DEFAULT current_timestamp); INSERT INTO haskell_postgres_migration (version) values (0);"
    putStrLn "Migration Table Created"

clearDB :: Connection -> IO ()
clearDB conn = do
    execute_ conn "DROP SCHEMA PUBLIC CASCADE; CREATE SCHEMA PUBLIC;"
    putStrLn "Cleared Database"

conn :: IO Connection
conn = do
    user <- getConf "POSTGRES_USER"
    password <- getConf "POSTGRES_PASSWORD"
    host <- getConf "POSTGRES_HOST"
    dbname <- getConf "POSTGRES_DBNAME"
    connectPostgreSQL $ genPostgreSQLConf $ ConfPostgreSQL user password host dbname


readFiles :: String -> [FilePath] -> IO ()
readFiles _ []  = return ()
readFiles dir (x:xs) = do
    content <- readFile $ dir ++ x
    putStrLn $ content
    readFiles dir xs

toQuery :: String -> Query
toQuery x = read $ show x ++ "":: Query

applyMigration :: Connection -> (Integer, FilePath) -> IO ()
applyMigration conn (version, path) = do
    sql <- readFile $ "migrations/" ++ path
    execute_ conn $ toQuery sql
    execute_ conn $ toQuery $ ("INSERT INTO haskell_postgres_migration (version) values (" ++ show version ++ ");")
    putStrLn $ "Migrating " ++ path
    return ()

applyMigrations :: Connection -> IO ()
applyMigrations conn = do
    last <- lastMigration conn
    allFiles <- migrationFiles last
    mapM_ (applyMigration conn) allFiles

main :: IO ()
main = do
    initial <- conn >>= isInitial
    if initial == False
       then do
           version <- conn >>= lastMigration
           putStrLn $ "Last successful migration version: " ++ show version
       else do
           putStrLn "Creating Migration Table"
           conn >>= createMigrationSchema
    putStrLn "Migrating...."
    conn >>= applyMigrations
    conn >>= close

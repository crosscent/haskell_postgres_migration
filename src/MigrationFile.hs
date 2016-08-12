module MigrationFile
    ( migrationFiles
    , filterFiles
    ) where

import Data.Maybe                   ( fromMaybe )
import System.Directory             ( getDirectoryContents )
import System.IO                    ( FilePath )

filterFiles :: [FilePath] -> [FilePath]
filterFiles [] = []
filterFiles (x:xs)
  | length x > 4 && (reverse . take 4 . reverse) x == ".sql"  = x : filterFiles xs
  | otherwise        = filterFiles xs

filterVersion :: Integer -> [FilePath] -> [(Integer, FilePath)]
filterVersion _ [] = []
filterVersion x (y:ys)
  | x < version = (version, y) : filterVersion x ys
  | otherwise             = filterVersion x ys
    where version = read $ take 4 y :: Integer

migrationFiles :: Integer -> IO [(Integer, FilePath)]
migrationFiles last = fmap (filterVersion last . filterFiles) $ getDirectoryContents ("migrations")

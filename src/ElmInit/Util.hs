module ElmInit.Util
  ( flattenMaybe
  , exists
  , getOr
  , enumerate
  ) where


import           Data.Maybe        (fromMaybe)
import           Data.Bool         (bool)
import           System.Directory  (doesFileExist, doesDirectoryExist)
import           Control.Exception (catch, IOException)


flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe = fromMaybe Nothing


exists :: FilePath -> IO Bool
exists =
  (>>=)
  <$> doesFileExist
  <*> (flip bool
        (return True)
        . doesDirectoryExist)


getOr :: Read a => a -> IO a
getOr =
  catch readLn . handler
  where
    handler :: a -> IOException -> IO a
    handler = const . return


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l

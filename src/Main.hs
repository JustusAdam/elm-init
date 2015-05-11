{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude hiding (FilePath)
import Control.Applicative ((<*>), pure)
import System.IO hiding (FilePath)
import Filesystem
import Filesystem.Path.CurrentOS as Path
import Control.Monad (fmap, liftM)
import qualified Data.ByteString as ByteString
import Data.FileEmbed


data Result = Success | Failiure String


standardDirectories = map decodeString [
    "src",
    "elm-stuff"
  ]

elmMain :: ByteString.ByteString
elmMain = $(embedFile "resources/Main.elm")

elmPackage :: ByteString.ByteString
elmPackage = $(embedFile "resources/elm-package.json")

standardFiles = map (\(x, y) -> (decodeString x, y) ) [
    ("src/Main.elm", Just elmMain),
    ("elm-package.json", Just elmPackage),
    ("README.md", Nothing)
  ]


exists :: FilePath -> IO Bool
exists f = isFile f >>= (\isFile' -> isDirectory f >>= (\isDir' -> return $ isFile' || isDir'))


mkFiles :: [(FilePath, Maybe ByteString.ByteString)] -> IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile :: FilePath -> Maybe ByteString.ByteString -> IO Result
mkFile name defaultFile = do
  e <- exists name
  if e then
    return $ Failiure $ "file " ++ encodeString name ++ " already exists"
  else
    System.IO.withFile (encodeString name) WriteMode $ \h ->
      maybe
        (return Success)
        (\f -> ByteString.hPut h f >> return Success)
        defaultFile


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ ( createTree . (wd </>))


main :: IO ()
main = do
  mkDirs (decodeString "./") standardDirectories
  res <- mkFiles standardFiles
  mapM_ (\r ->
      case r of
        Success -> return ()
        Failiure message -> putStrLn message
    ) res

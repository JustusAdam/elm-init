{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Applicative       (pure, (<*>))
import qualified Control.Arrow             as Arrow (first)
import           Data.ByteString           as ByteString (ByteString, hPut)
import           Data.FileEmbed            (embedFile)
import           Filesystem                (createTree, isDirectory, isFile)
import           Filesystem.Path.CurrentOS as Path
import           Prelude                   hiding (FilePath)
import           System.IO                 (withFile, IOMode(WriteMode))


data Result = Success | Failiure String


standardDirectories = map decodeString [
    "src",
    "elm-stuff"
  ]


standardFiles = map (Arrow.first decodeString) [
    ("src/Main.elm", Just $(embedFile "resources/Main.elm")),
    ("elm-package.json", Just $(embedFile "resources/elm-package.json")),
    ("README.md", Nothing)
  ]


exists :: FilePath -> IO Bool
exists f = do
  isF   <- isFile f
  isDir <- isDirectory f
  return $ isF || isDir


mkFiles :: [(FilePath, Maybe ByteString)] -> IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile :: FilePath -> Maybe ByteString -> IO Result
mkFile name defaultFile = do
  e <- exists name
  if e then
    return $ Failiure $ "file " ++ encodeString name ++ " already exists"
  else do
    System.IO.withFile (encodeString name) WriteMode $ \h ->
      maybe (return ()) (ByteString.hPut h) defaultFile

    return Success


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ ( createTree . (wd </>))


main :: IO ()
main = do
  mkDirs (decodeString "./") standardDirectories
  res <- mkFiles standardFiles
  mapM_ (\r ->
      case r of
        Success           -> return ()
        Failiure message  -> putStrLn message
    ) res

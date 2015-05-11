{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Applicative       (pure, (<*>))
import qualified Control.Arrow             as Arrow (first)
import           Data.ByteString           as ByteString (ByteString, hPut)
import           Data.FileEmbed            (embedFile)
import           Filesystem                (createTree, isDirectory, isFile, getWorkingDirectory)
import           Filesystem.Path.CurrentOS as Path
import           Prelude                   hiding (FilePath)
import           System.IO                 (IOMode (WriteMode), withFile)
import qualified Text.JSON                 as JSON
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Exception (catch, Exception, IOException)


type Result = Either String ()


standardDirectories = map decodeString [
    "elm-stuff"
  ]


standardFiles = map (Arrow.first decodeString) [
    ("elm-package.json", Just $(embedFile "resources/elm-package.json")),
    ("README.md", Nothing),
    ("LICENSE", Nothing)
  ]


standardSourceFiles = map (Arrow.first decodeString) [
    ("Main.elm", Just $(embedFile "resources/Main.elm"))
  ]


{-
  embedding a file as String

  import Data.Binary

  file :: String
  file = decode $(embedFile "filepath")
-}


sourceFolders = [
    "src",
    "lala"
  ]


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l


askChoices :: String -> Int -> [String] -> IO String
askChoices m s l = askChoices' m s l >>= (\i -> return $ l !! i)


getEither :: Read a => a -> IO a
getEither x = do
  catch readLn (handler x)
  where
    handler :: a -> IOException -> IO a
    handler x = const (return x)


askChoices' :: String -> Int -> [String] -> IO Int
askChoices' message selected choices = do
  putStrLn message
  let (l1, l2) = splitAt selected choices
  let (selectedElem : l2tail) = l2
  let out = intercalate "\n" (normFormat 1 l1 ++ (selectedFormat selected selectedElem : normFormat (selected + 1) l2tail))

  ask out

  where
    enumF x = ((show x) ++ " )  ")
    enumFn = (("    " ++).enumF)
    enumFs = (("  * " ++).enumF)
    normFormat f l = map ((uncurry (++)).(Arrow.first enumFn)) $ enumerate f l
    selectedFormat x y = ((++ y).enumFs) x

    ask out = do
          putStrLn out
          -- apparently using putStr here doe not print the full string but
          -- omits the last line ... buffering?
          i <- getEither selected

          if i <= (length choices) then
            return i
          else do
            putStrLn "invalid choice, please choose again"
            ask out


askChoicesWithOther :: String -> Int -> [String] -> IO String
askChoicesWithOther m s l = do
  i <- askChoices' m s (l ++ ["other (specify)"])
  if i == (length l) then
    getAlternative
  else
    return $ l !! i

  where
    verifyValidity = const True
    getAlternative = do
      putStrLn "please enter an alternative"
      s <- getLine
      if verifyValidity s then
        return s
      else
        getAlternative


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
    return $ Left $ "file " ++ encodeString name ++ " already exists"
  else do
    System.IO.withFile (encodeString name) WriteMode $ \h ->
      maybe (return ()) (ByteString.hPut h) defaultFile

    return $ Right ()


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles sourceFolder = mkFiles $ map (Arrow.first (sourceFolder </>)) standardSourceFiles


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ ( createTree . (wd </>))


main :: IO ()
main = do

  wd <- getWorkingDirectory

  srcFolder <- fmap ((wd </>).decodeString) (askChoicesWithOther "choose a source folder name" 0 sourceFolders)

  -- putStrLn srcFolder

  mkDirs wd (srcFolder : standardDirectories)
  resStatic <- mkFiles standardFiles

  resSource <- mkSourceFiles srcFolder

  mapM_ (\r ->
      case r of
        Right _       -> return ()
        Left message  -> putStrLn message
    ) (resStatic ++ resSource)

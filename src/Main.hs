{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Applicative       (pure, (<*>))
import qualified Control.Arrow             as Arrow (first, second)
import           Control.Exception         (Exception, IOException, catch)
import           Data.Bool                 (bool)
import           Data.ByteString           as ByteString (ByteString, hPut)
import           Data.FileEmbed            (embedFile)
import           Data.List                 (intercalate)
import           Data.Maybe                (fromMaybe)
import           Filesystem                (createTree, getWorkingDirectory,
                                            isDirectory, isFile)
import           Filesystem.Path.CurrentOS as Path (FilePath, decodeString,
                                                    encodeString, (</>))
import           Prelude                   hiding (FilePath)
import qualified System.Environment        as Environment (getArgs)
import           System.IO                 (IOMode (WriteMode), withFile)
import           Text.JSON                 as JSON (JSON, JSObject,
                                                    JSValue (JSArray, JSObject),
                                                    makeObj, showJSON,
                                                    toJSObject)


type Result = Either String ()


data CmdArgs = CmdArgs {
    workingDirectory :: FilePath
  }


data Version = Version {
    majorVersion :: Int,
    minorVersion :: Int,
    fineVersion  :: Int
  }


data UserDecisions = Default {
    sourceFolder :: FilePath,
    version      :: Version,
    summary      :: String,
    repository   :: String,
    license      :: String
  }


instance Show Version where
  showsPrec p (Version ma mi fi) = shows ma . showChar '.' . shows mi . showChar '.' . shows fi


instance Read Version where
  readsPrec p s =
    -- this is supposed to do read "1.2.3" => Version 1 2 3
    [(Version ma mi fi, r) |
      (ma, '#':r2) <- reads s :: [(Int, String)],
      (mi, '#':r3) <- reads r2 :: [(Int, String)],
      (fi, r) <- reads r3 :: [(Int, String)]
      ]
    -- previous version (also does not work)
    -- [(Version ma mi fi, r) |
    --   (ma, '.', mi, '.', fi) <- reads s
    --   ]


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
    "src"
  ]


elmPackageJSON :: UserDecisions -> JSValue
elmPackageJSON d@(Default {sourceFolder=sourceFolder, version=version, summary=summary, license=license, repository=repository}) =
  makeObj $ (
    ("dependencies", JSObject $ toJSObject $ map (Arrow.second showJSON) [
      ("elm-lang/core", "2.0.0 <= v < 3.0.0"),
      ("evancz/elm-html", "3.0.0 <= v < 4.0.0")
      ]
    )
  ) : (map (Arrow.second showJSON) [
    ("version", show version),
    ("summary", summary),
    ("repository", repository),
    ("license", license),
    ("elm-version", "0.15.0 <= v < 0.16.0")
  ]) ++ (map (Arrow.second showJSON) [
    ("source-directories", [
      showJSON $ encodeString sourceFolder
      ]),
    ("exposed-modules", [])
    ]
  )


emptyDecisions :: UserDecisions
emptyDecisions =
  Default {
    summary       = "",
    repository    = "https://github.com/JustusAdam/elm-init.git",
    version       = Version 1 0 0,
    license       = "BSD3",
    sourceFolder  = decodeString "src"
  }


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l


askChoices :: String -> Int -> [String] -> IO String
askChoices m s l = askChoices' m s l >>= (\i -> return $ l !! i)


getEither :: Read a => a -> IO a
getEither x =
  Control.Exception.catch readLn (handler x)
  where
    handler :: a -> IOException -> IO a
    handler = const . return


askChoices' :: String -> Int -> [String] -> IO Int
askChoices' message selected choices = do
  putStrLn message
  let (l1, l2) = splitAt selected choices
  let (selectedElem : l2tail) = l2
  let out = intercalate "\n" (normFormat 1 l1 ++ (selectedFormat selected selectedElem : normFormat (selected + 1) l2tail))

  ask out

  where
    enumF x = show x ++ " )  "
    enumFn = ("    " ++).enumF
    enumFs = ("  * " ++).enumF
    normFormat f l = map (uncurry (++) . Arrow.first enumFn) $ enumerate f l
    selectedFormat x y = ((++ y).enumFs) x

    ask out = do
          putStrLn out
          -- apparently using putStr here doe not print the full string but
          -- omits the last line ... buffering?
          i <- getEither selected

          if i <= length choices then
            return i
          else do
            putStrLn "invalid choice, please choose again"
            ask out


askChoicesWithOther :: String -> Int -> [String] -> IO String
askChoicesWithOther m s l = do
  i <- askChoices' m s (l ++ ["other (specify)"])
  if i == length l then
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
mkFile name defaultFile = exists name >>=
  bool
    (return $ Left $ "file " ++ encodeString name ++ " already exists")
    (
      System.IO.withFile
        (encodeString name)
        WriteMode
        (\h -> maybe (return ()) (ByteString.hPut h) defaultFile)
      >>
      return (Right ())
      )


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles sourceFolder = mkFiles $ map (Arrow.first (sourceFolder </>)) standardSourceFiles


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ ( createTree . (wd </>))


getCmdArgs :: IO CmdArgs
getCmdArgs =
  Environment.getArgs >>=
    (\args ->
      if length args > 1
        then
          return $ decodeString $ head args
        else
          getWorkingDirectory
    ) >>=
        \wd -> return CmdArgs { workingDirectory = wd }



main :: IO ()
main = do

  wd        <- fmap workingDirectory getCmdArgs

  srcFolder <- fmap ((wd </>).decodeString) (askChoicesWithOther "choose a source folder name" 0 sourceFolders)

  projectName <- askChoicesWithOther "project name?" 0 [encodeString wd]

  -- putStrLn srcFolder

  mkDirs wd (srcFolder : standardDirectories)
  resStatic <- mkFiles standardFiles

  resSource <- mkSourceFiles srcFolder

  mapM_ (\r ->
      case r of
        Right _       -> return ()
        Left message  -> putStrLn message
    ) (resStatic ++ resSource)

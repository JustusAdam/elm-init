{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Applicative      (pure, (<*>))
import qualified Control.Arrow            as Arrow (first, second)
import           Control.Exception        (Exception, IOException, catch)
import           Data.Aeson               as Aeson (ToJSON, Value, object,
                                                    toJSON, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString          as ByteString (ByteString, hPut,
                                                         putStrLn)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.FileEmbed           (embedFile)
import           Data.List                (intercalate)
import           Data.Maybe               (fromMaybe)
import           Data.Text                as Text (Text, append, intercalate,
                                                   pack, splitOn, unpack)
import           Data.Text.IO             as TextIO (getLine, putStrLn)
import           Data.Traversable         (sequenceA)
import           Prelude                  hiding (getLine, putStrLn)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory)
import qualified System.Environment       as Environment (getArgs)
import           System.FilePath          (takeBaseName, (</>))
import           System.IO                (IOMode (WriteMode), withFile)


type Result = Either Text ()


data CmdArgs = CmdArgs {
    workingDirectory :: FilePath
  }


data Version = Version {
    majorVersion :: Int,
    minorVersion :: Int,
    fineVersion  :: Int
  }


data UserDecisions = Default {
    projectName  :: Text,
    sourceFolder :: FilePath,
    version      :: Version,
    summary      :: Text,
    repository   :: Text,
    license      :: Text,
    elmVersion   :: Text
  }


data ElmPackage = ElmPackage {
    pkgVersion        :: Version,
    pkgSummary        :: Text,
    pkgRepository     :: Text,
    pkgLicense        :: Text,
    pkgDependencies   :: Aeson.Value,
    pkgExposedModules :: [Text],
    pkgElmVersion     :: Text
  }


instance Aeson.ToJSON ElmPackage where
  toJSON = object . sequenceA [
    ("version" .=)            . show . pkgVersion
    , ("summary" .=)          . pkgSummary
    , ("repository" .=)       . pkgRepository
    , ("license" .=)          . pkgLicense
    , ("dependencies" .=)     . pkgDependencies
    , ("exposed-modules" .=)  . pkgExposedModules
    , ("elm-version" .=)      . pkgElmVersion
    ]


instance Show Version where
  showsPrec p (Version ma mi fi) =
    shows ma . showChar '.' . shows mi . showChar '.' . shows fi


-- instance Read Version where
--   readsPrec p s =
--     -- this is supposed to do read "1.2.3" => Version 1 2 3
--     [(Version ma mi fi, r) |
--       (ma, '.':r2) <- reads s :: [(Int, String)],
--       (mi, '.':r3) <- reads r2 :: [(Int, String)],
--       (fi, r) <- reads r3 :: [(Int, String)]
--       ]
    -- previous version (also does not work)
    -- [(Version ma mi fi, r) |
    --   (ma, '.', mi, '.', fi) <- reads s
    --   ]


standardDirectories = [
    "elm-stuff"
  ]


standardFiles = [
    ("README.md", Nothing),
    ("LICENSE", Nothing)
  ]


standardSourceFiles = [
    ("Main.elm", Just $(embedFile "resources/Main.elm"))
  ]


defaultProjectVersion = Version 1 0 0


defaultElmVersion = "0.15.0 <= v < 0.16.0"


defaultLicenses = [
    "None",
    "BSD3",
    "LGPL3",
    "LGPL2",
    "MIT",
    "Apache"
  ] :: [Text]


{-
  embedding a file as String

  import Data.Binary

  file :: String
  file = decode $(embedFile "filepath")
-}


sourceFolders = [
    "src"
  ]


elmConfigName = "elm-package.json" :: FilePath


makePackage :: UserDecisions -> ElmPackage
makePackage = pure ElmPackage
  <*> version
  <*> summary
  <*> repository
  <*> license
  <*> const (object [])
  <*> const []
  <*> elmVersion



emptyDecisions :: UserDecisions
emptyDecisions =
  Default {
    summary       = "",
    repository    = "",
    version       = Version 1 0 0,
    license       = "",
    sourceFolder  = "src",
    projectName   = "",
    elmVersion    = defaultElmVersion
  }


getUserDecisions :: FilePath -> IO UserDecisions
getUserDecisions wd =
  pure Default
  <*> askChoicesWithOther "project name?" 0 (const True) [pack $ takeBaseName wd]
  <*> (fmap ((wd </>).unpack) (askChoicesWithOther "choose a source folder name" 0 (const True) (map pack sourceFolders)))
  <*> fmap versionFromString (askChoicesWithOther "initial project version?" 0 (const True) [pack $ show defaultProjectVersion])
  <*> (putStrLn "a quick summary" >> getLine)
  <*> (putStrLn "project repository url" >> getLine)
  <*> askChoicesWithOther "choose a license" 0 (const True) defaultLicenses
  <*> askChoicesWithOther "select the elm-version" 0 (const True) [defaultElmVersion]


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l


splitL :: Eq a => a -> [a] -> [[a]]
splitL _ [] = []
splitL se l = reverse $ foldl (helper se) [[]] l
  where
    helper :: Eq a => a -> [[a]] -> a -> [[a]]
    helper se [] e = helper se [[]] e
    helper se acc@(x:xs) e
      | se == e   = []:acc
      | otherwise = (e:x):xs


versionFromString :: Text -> Version
versionFromString s
  | length sp /= 3  = error "Parse failed"
  | otherwise       = Version ma mi fi
  where
    sp@(ma:mi:fi:[]) = map (read.unpack) (splitOn "." s) :: [Int]



askChoices :: Text -> Int -> [Text] -> IO Text
askChoices m s l = askChoices' m s l >>= (\i -> return $ l !! i)


getEither :: Read a => a -> IO a
getEither x =
  Control.Exception.catch readLn (handler x)
  where
    handler :: a -> IOException -> IO a
    handler = const . return


askChoices' :: Text -> Int -> [Text] -> IO Int
askChoices' message selected choices = do
  TextIO.putStrLn message
  ask out

  where
    (l1, (selectedElem : l2tail)) = splitAt selected choices
    out =
      Text.intercalate
        "\n"
        (normFormat 1 l1
        ++ (selectedFormat selected selectedElem : normFormat (selected + 1) l2tail))

    enumF x = append (pack $ show x) " )  "
    enumFn = (append "    ").enumF
    enumFs = (append "  * ").enumF
    normFormat f l = map ((uncurry append) . Arrow.first enumFn) $ enumerate f l
    selectedFormat x y = ((`append` y).enumFs) x

    ask out = do
          TextIO.putStrLn out
          -- apparently using putStr here doe not print the full string but
          -- omits the last line ... buffering?
          i <- getEither selected

          if i <= length choices then
            return i
          else do
            TextIO.putStrLn "invalid choice, please choose again"
            ask out


askChoicesWithOther :: Text -> Int -> (Text -> Bool) -> [Text] -> IO Text
askChoicesWithOther m s verifier l = do
  i <- askChoices' m s (l ++ ["other (specify)"])
  if i == length l then
    getAlternative
  else
    return $ l !! i

  where
    getAlternative = do
      TextIO.putStrLn "please enter an alternative"
      s <- getLine
      if verifier s then
        return s
      else
        TextIO.putStrLn "Invalid input, plese enter again" >>
        getAlternative


exists :: FilePath -> IO Bool
exists f = do
  isF   <- doesFileExist f
  isDir <- doesDirectoryExist f
  return $ isF || isDir


mkFiles :: [(FilePath, Maybe ByteString.ByteString)] -> IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile :: FilePath -> Maybe ByteString.ByteString -> IO Result
mkFile name defaultFile = exists name >>=
  bool
    (
      withFile
        name
        System.IO.WriteMode
        (\h -> maybe (return ()) (ByteString.hPut h) defaultFile)
      >>
      return (Right ())
      )
    (return $ Left $ "file " `append` pack name `append` " already exists")


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles sourceFolder = mkFiles $ map (Arrow.first (sourceFolder </>)) standardSourceFiles


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ ((createDirectoryIfMissing True) . (wd </>))


writeConf :: FilePath -> UserDecisions -> IO ()
writeConf wd dec = withFile (wd </> elmConfigName) System.IO.WriteMode (`LBS.hPut` (encodePretty $ makePackage dec))


getCmdArgs :: IO CmdArgs
getCmdArgs =
  Environment.getArgs >>=
    (\args ->
      if length args > 1
        then
          return $ head args
        else
          getCurrentDirectory
    ) >>=
        \wd -> return CmdArgs { workingDirectory = wd }



main :: IO ()
main = do

  wd        <- fmap workingDirectory getCmdArgs

  decisions <- getUserDecisions wd

  mkDirs wd (sourceFolder decisions : standardDirectories)
  resStatic <- mkFiles standardFiles

  resSource <- mkSourceFiles $ sourceFolder decisions

  writeConf wd decisions

  mapM_ (\r ->
      case r of
        Right _       -> return ()
        Left message  -> TextIO.putStrLn message
    ) (resStatic ++ resSource)

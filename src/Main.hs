{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Control.Applicative      (pure, (<*>))
import qualified Control.Arrow            as Arrow (first)
import           Control.Exception        (IOException, catch)
import           Data.Aeson               as Aeson (ToJSON, Value, object,
                                                    toJSON, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString          as ByteString (ByteString, empty,
                                                         hPut, putStrLn)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.FileEmbed           (embedFile)
import           Data.Text                as Text (Text, append, intercalate,
                                                   pack, splitOn, unpack, toLower)
import           Data.Text.IO             as TextIO (getLine, putStrLn)
import           Data.Traversable         (sequenceA)
import           Prelude                  hiding (getLine, putStrLn)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory, makeAbsolute)
import           System.Environment       (getArgs)
import           System.FilePath          (isValid, takeBaseName, (</>))
import           System.IO                (IOMode (WriteMode), withFile)


type Result = Either Text ()


data CmdArgs = CmdArgs { workingDirectory :: FilePath }


data Version = Version { majorVersion :: Int
                       , minorVersion :: Int
                       , fineVersion  :: Int
                       }


data UserDecisions = Default { projectName  :: Text
                             , sourceFolder :: FilePath
                             , version      :: Version
                             , summary      :: Text
                             , repository   :: Text
                             , license      :: Text
                             , elmVersion   :: Text
                             }


data ElmPackage = ElmPackage { pkgVersion        :: Version
                             , pkgSummary        :: Text
                             , pkgRepository     :: Text
                             , pkgLicense        :: Text
                             , pkgDependencies   :: Aeson.Value
                             , pkgExposedModules :: [Text]
                             , pkgElmVersion     :: Text
                             }


instance Aeson.ToJSON ElmPackage where
  toJSON = object . sequenceA
    [ ("version" .=)          . show . pkgVersion
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


standardDirectories =
  [ "elm-stuff" ]


standardFiles =
  [ ("README.md", Nothing)
  , ("LICENSE", Nothing)
  ]


standardSourceFiles =
  [ ("Main.elm", Just $(embedFile "resources/Main.elm")) ]


standardLicenses =
  [ ("None", ByteString.empty)
  , ("BSD3", $(embedFile "resources/licenses/BSD3"))
  , ("LGPL3", $(embedFile "resources/licenses/LGPL3"))
  , ("LGPL2", $(embedFile "resources/licenses/LGPL2"))
  , ("MIT", $(embedFile "resources/licenses/MIT"))
  , ("Apache", $(embedFile "resources/licenses/Apache"))
  , ("GPLv2", $(embedFile "resources/licenses/GPLv2"))
  , ("GPLv3", $(embedFile "resources/licenses/GPLv3"))
  ] :: [(Text, ByteString.ByteString)]


defaultProjectVersion = Version 1 0 0


defaultElmVersion = "0.15.0 <= v < 0.16.0"


defaultLicenses = fst $ unzip standardLicenses


{-
  embedding a file as String

  import Data.Binary

  file :: String
  file = decode $(embedFile "filepath")
-}


sourceFolders =
  [ "src" ]


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
  Default { summary       = ""
          , repository    = ""
          , version       = Version 1 0 0
          , license       = ""
          , sourceFolder  = "src"
          , projectName   = ""
          , elmVersion    = defaultElmVersion
          }


getUserDecisions :: FilePath -> IO UserDecisions
getUserDecisions wd =
  pure Default
  <*> askChoicesWithOther
        "project name?"
        0
        (const True)
        [pack $ takeBaseName wd]
  <*> fmap
        ((wd </>) . unpack)
        (askChoicesWithOther
          "choose a source folder name"
          0
          (isValid . unpack)  -- filepath path verifier
          (map pack sourceFolders))
  <*> fmap
        versionFromString
        (askChoicesWithOther
          "initial project version?"
          0
          (const True)  -- TODO add verifier
          [pack $ show defaultProjectVersion])
  <*> (putStrLn "a quick summary" >> getLine)
  <*> (putStrLn "project repository url" >> getLine)
  <*> askChoicesWithOther
        "choose a license"
        0
        (const True)
        defaultLicenses
  <*> askChoicesWithOther
        "select the elm-version"
        0
        (const True)  -- add verifier?
        [defaultElmVersion]


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
  | length sp /= 3  = error "Parse failed"  -- I'm so sorry
  | otherwise       = Version ma mi fi
  where
    sp@[ma,mi,fi] = map (read.unpack) (splitOn "." s) :: [Int]



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
  putStrLn message
  ask out

  where
    (l1, selectedElem : l2tail) = splitAt selected choices
    out =
      intercalate
        "\n"
        (normFormat 1 l1
          ++ (selectedFormat selected selectedElem : normFormat (selected + 1) l2tail))

    enumF x = append (pack $ show x) " )  "
    enumFn = append "    " . enumF
    enumFs = append "  * " . enumF
    normFormat f = map (uncurry append . Arrow.first enumFn) . enumerate f
    selectedFormat x y = (flip append y . enumFs) x

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


askChoicesWithOther :: Text -> Int -> (Text -> Bool) -> [Text] -> IO Text
askChoicesWithOther m s verifier l = do
  i <- askChoices' m s (l ++ ["other (specify)"])
  if i == length l then
    getAlternative
  else
    return $ l !! i

  where
    getAlternative = do
      putStrLn "please enter an alternative"

      s <- getLine
      if verifier s then
        return s
      else
        putStrLn "Invalid input, plese enter again" >>
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
    (withFile
      name
      System.IO.WriteMode
      (\h -> maybe (return ()) (ByteString.hPut h) defaultFile)
    >> return (Right ()))
    (return $ Left $ "file " `append` pack name `append` " already exists")


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles sourceFolder =
  mkFiles $ map
              (Arrow.first (sourceFolder </>))
              standardSourceFiles


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ (createDirectoryIfMissing True . (wd </>))


writeConf :: FilePath -> UserDecisions -> IO ()
writeConf wd dec =
  withFile
    (wd </> elmConfigName)
    WriteMode
    (flip LBS.hPut $ encodePretty $ makePackage dec)


putLicense :: FilePath -> Text -> IO Result
putLicense wd =
  maybe
    (return $ Left "License file not found")
    (fmap
      Right
      . withFile
          (wd </> "LICENSE")
          WriteMode
      . flip ByteString.hPut)
  . flip lookup standardLicenses


getCmdArgs :: IO CmdArgs
getCmdArgs =
  fmap CmdArgs
    (getArgs >>=
      (\args ->
        case args of
          []  -> getCurrentDirectory
          [x] -> makeAbsolute x
          _   -> error "Too many arguments"))  -- I'm so sorry



verifyWD :: FilePath -> IO FilePath
verifyWD wd =
  doesFileExist wd >>=
    bool
      (error "The chosen directory is a file") -- I'm so sorry
      (doesDirectoryExist wd >>=
        bool
          (return wd)
          (putStrLn "the chosen directory does not exist yet, shall I create it? [y/n]"
          >> getResp >>=
            (bool
              makeDirs
              (error "the chosen directory does not exist"))
          >> return wd))

  where
    getResp :: IO Bool
    getResp = fmap (`elem` ["y", "yes"]) getLine

    makeDirs = createDirectoryIfMissing True wd -- >> return wd



main :: IO ()
main = do

  -- get either the working directory or the directory the user entered
  wd        <- getCmdArgs >>= (verifyWD . workingDirectory)

  -- ask all important input first
  decisions <- getUserDecisions wd

  -- create necessary directories
  mkDirs wd (sourceFolder decisions : standardDirectories)

  -- create non-dynamic files, collect errors
  resStatic <- mkFiles $ map (Arrow.first (wd </>)) standardFiles

  -- create Elm source files, collect errors
  resSource <- mkSourceFiles $ wd </> sourceFolder decisions

  -- write the package config based on the user decisions
  writeConf wd decisions

  -- write the choosen license
  putLicense wd (license decisions)

  -- report all errors
  mapM_ (either putStrLn return) (resStatic ++ resSource)

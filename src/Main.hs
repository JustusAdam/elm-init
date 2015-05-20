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
import qualified Data.ByteString          as ByteString (ByteString, hPut)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.FileEmbed           (embedFile)
import           Data.Maybe               (fromMaybe)
import           Data.Text                as Text (Text, append, intercalate,
                                                   pack, splitOn, unpack)
import           Data.Text.IO             as TextIO (getLine, putStrLn)
import           Data.Traversable         (sequenceA)
import           Prelude                  hiding (getLine, putStrLn)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory, makeAbsolute)
import           System.Environment       (getArgs)
import           System.FilePath          (isValid, takeBaseName, (</>))
import           System.IO                (IOMode (WriteMode), withFile)


standardDirectories :: [FilePath]
standardDirectories = [ "elm-stuff" ]

standardSourceFolders :: [FilePath]
standardSourceFolders = [ "src" ]

standardFiles :: [(FilePath, Maybe ByteString.ByteString)]
standardFiles = [ ("README.md", Nothing) ]

standardSourceFiles :: [(FilePath, Maybe ByteString.ByteString)]
standardSourceFiles = [ ("Main.elm", Just $(embedFile "resources/Main.elm")) ]

standardLicenses :: [(Text, Maybe ByteString.ByteString)]
standardLicenses =
  [ ("None"   , Nothing                                       )
  , ("BSD3"   , Just $(embedFile "resources/licenses/BSD3"   ))
  , ("LGPL3"  , Just $(embedFile "resources/licenses/LGPL3"  ))
  , ("LGPL2"  , Just $(embedFile "resources/licenses/LGPL2"  ))
  , ("MIT"    , Just $(embedFile "resources/licenses/MIT"    ))
  , ("Apache" , Just $(embedFile "resources/licenses/Apache" ))
  , ("GPLv2"  , Just $(embedFile "resources/licenses/GPLv2"  ))
  , ("GPLv3"  , Just $(embedFile "resources/licenses/GPLv3"  ))
  ]

defaultProjectVersion :: Version
defaultProjectVersion = Version 1 0 0

defaultElmVersion :: Text
defaultElmVersion = "0.15.0 <= v < 0.16.0"

availableLicenses :: [Text]
availableLicenses = fst $ unzip standardLicenses

elmConfigName :: FilePath
elmConfigName = "elm-package.json"


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
  showsPrec _ (Version ma mi fi) =
    shows ma . showChar '.' . shows mi . showChar '.' . shows fi


versionFromString :: Text -> Version
versionFromString s
  | length sp /= 3  = error "Parse failed"  -- I'm so sorry
  | otherwise       = Version ma mi fi
  where
    sp@[ma,mi,fi] = map (read.unpack) (splitOn "." s) :: [Int]


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


makePackage :: UserDecisions -> ElmPackage
makePackage = pure ElmPackage
  <*> version
  <*> summary
  <*> repository
  <*> license
  <*> const (object [])
  <*> const []
  <*> elmVersion


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l


getEither :: Read a => a -> IO a
getEither x =
  catch readLn (handler x)
  where
    handler :: a -> IOException -> IO a
    handler = const . return


askChoices :: Text -> Int -> [Text] -> IO Text
askChoices m s l = fmap (l !!) (askChoices' m s l)


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
    normFormat = (map (uncurry append . Arrow.first enumFn) .) . enumerate
    selectedFormat x y = (flip append y . enumFs) x

    ask aout = do
          putStrLn aout
          -- apparently using putStr here doe not print the full string but
          -- omits the last line ... buffering?
          i <- getEither selected

          if i <= length choices then
            return i
          else do
            putStrLn "invalid choice, please choose again"
            ask aout


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

      str <- getLine
      if verifier str then
        return str
      else
        putStrLn "Invalid input, plese enter again" >>
        getAlternative


exists :: FilePath -> IO Bool
exists f =
  doesFileExist f >>=
  bool
    (doesDirectoryExist f)
    (return True)


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
      (doesDirectoryExist wd >>=
        bool
          (putStrLn "the chosen directory does not exist yet, shall I create it? [y/n]"
          >> getResp >>=
            bool
              (error "the chosen directory does not exist")  -- I'm so sorry
              makeDirs
          >> return wd)
          (return wd))
      (error "The chosen directory is a file") -- I'm so sorry

  where
    getResp :: IO Bool
    getResp = fmap (flip elem ["y", "yes"]) getLine

    makeDirs = createDirectoryIfMissing True wd


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
          (map pack standardSourceFolders))
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
        availableLicenses
  <*> askChoicesWithOther
        "select the elm-version"
        0
        (const True)  -- add verifier?
        [defaultElmVersion]


mkFiles :: [(FilePath, Maybe ByteString.ByteString)] -> IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile :: FilePath -> Maybe ByteString.ByteString -> IO Result
mkFile name defaultFile = exists name >>=
  bool
    (withFile
      name
      WriteMode
      (\h -> maybe (return ()) (ByteString.hPut h) defaultFile)
    >> return (Right ()))
    (return $ Left $ "file " `append` pack name `append` " already exists")


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles sf =
  mkFiles $ map (Arrow.first (sf </>)) standardSourceFiles


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs wd = mapM_ (createDirectoryIfMissing True . (wd </>))


writeConf :: FilePath -> UserDecisions -> IO ()
writeConf wd dec =
  withFile
    (wd </> elmConfigName)
    WriteMode
    (flip LBS.hPut $ encodePretty $ makePackage dec)


flattenMaybe :: Maybe (Maybe a) -> Maybe a
flattenMaybe = fromMaybe Nothing

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
  . flattenMaybe . flip lookup standardLicenses


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
  _         <- putLicense wd (license decisions)

  -- report all errors
  mapM_ (either putStrLn return) (resStatic ++ resSource)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where


import           Control.Applicative      (pure, (<*>))
import qualified Control.Arrow            as Arrow (first)
import           Control.Exception        (IOException, catch)
import           Control.Monad            ((>=>), (<=<))
import           Data.Aeson               as Aeson (ToJSON, Value, object,
                                                    toJSON, (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Data.Bool                (bool)
import qualified Data.ByteString          as ByteString (ByteString, hPut)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.FileEmbed           (embedFile)
import           Data.Functor             ((<$>))
import           Data.Maybe               (fromMaybe, isJust)
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
import           Data.Version             (parseVersion, Version(..), showVersion,
                                          makeVersion)


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
defaultProjectVersion = makeVersion [1, 0, 0]

defaultElmVersion :: Text
defaultElmVersion = "0.15.0 <= v < 0.16.0"

availableLicenses :: [Text]
availableLicenses = fst $ unzip standardLicenses

elmConfigName :: FilePath
elmConfigName = "elm-package.json"


type Result = Either Text ()


data CmdArgs = CmdArgs { workingDirectory :: FilePath }


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
                             , pkgSourceDirs     :: [Text]
                             }


readVersion :: String -> [(Version, String)]
readVersion = readP_to_S parseVersion


instance Aeson.ToJSON ElmPackage where
  toJSON = object . sequenceA
    [ ("version" .=)          . showVersion . pkgVersion
    , ("summary" .=)          . pkgSummary
    , ("repository" .=)       . pkgRepository
    , ("license" .=)          . pkgLicense
    , ("dependencies" .=)     . pkgDependencies
    , ("exposed-modules" .=)  . pkgExposedModules
    , ("elm-version" .=)      . pkgElmVersion
    , ("source-directories".=). pkgSourceDirs
    ]


emptyDecisions :: UserDecisions
emptyDecisions =
  Default { summary       = ""
          , repository    = ""
          , version       = makeVersion [0, 0, 0]
          , license       = ""
          , sourceFolder  = ""
          , projectName   = ""
          , elmVersion    = ""
          }


makePackage :: UserDecisions -> ElmPackage
makePackage = ElmPackage
  <$> version
  <*> summary
  <*> repository
  <*> license
  <*> const (object [])
  <*> const []
  <*> elmVersion
  <*> (:[]) . pack . sourceFolder


enumerate :: Int -> [a] -> [(Int,a)]
enumerate from l = zip [from..(length l)] l


getEither :: Read a => a -> IO a
getEither =
  catch readLn . handler
  where
    handler :: a -> IOException -> IO a
    handler = const . return


askChoices :: Text -> Int -> [Text] -> IO Text
askChoices =
  ((fmap <$> (!!) <*>) .) . askChoices'


askChoices' :: Text -> Int -> [Text] -> IO Int
askChoices' message selected choices =
  putStrLn message >>
  ask out

  where
    (l1, selectedElem : l2tail) = splitAt selected choices
    out =
      intercalate
        "\n"
        (normFormat 1 l1
          ++ (selectedFormat selected selectedElem : normFormat (selected + 1) l2tail))

    enumF = flip append " )  " . pack . show
    enumFn = append "    " . enumF
    enumFs = append "  * " . enumF
    normFormat = (map (uncurry append . Arrow.first enumFn) .) . enumerate
    selectedFormat x y = (flip append y . enumFs) x

    ask =
          (>>)
          <$> putStrLn
          <*> (\a ->
                getEither selected >>=
                  (bool
                    (putStrLn "invalid choice, please choose again" >>
                    ask a)
                    <$> return
                    <*> (<= length choices)))


askChoicesWithOther :: Text -> Int -> (Text -> Maybe a) -> [Text] -> IO a
askChoicesWithOther m s trans l =
    askChoices' m s (l ++ ["other (specify)"])
    >>= (flip bool getAlternative
          <$> maybe (error "No parse") return . trans . (l !!)
          <*> (== length l))

  where
    getAlternative =
      putStrLn "please enter an alternative" >>
      getLine >>=
        (maybe
          (putStrLn "Invalid input, plese enter again" >>
          getAlternative)
          return
          . trans)


exists :: FilePath -> IO Bool
exists =
  (>>=)
  <$> doesFileExist
  <*> (flip bool
        (return True)
        . doesDirectoryExist)


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
    getResp = flip elem ["y", "yes"] <$> getLine

    makeDirs = createDirectoryIfMissing True wd


readOneVersion :: String -> Maybe Version
readOneVersion = verif . readVersion
  where
    verif ((v, []):_) = Just v
    verif (_:[])      = Nothing
    verif (_:xs)      = verif xs

verifyElmVersion :: String -> Maybe Version
verifyElmVersion = hasElmStructure <=< readOneVersion
  where
    hasElmStructure v@(Version [ _, _, _ ] []) = Just v
    hasElmStructure _                          = Nothing


getUserDecisions :: FilePath -> IO UserDecisions
getUserDecisions wd =
  Default
  <$> askChoicesWithOther
        "project name?"
        0
        (Just)
        [pack $ takeBaseName wd]
  <*> askChoicesWithOther
        "choose a source folder name"
        0
        ((bool Nothing <$> Just <*> isValid) . unpack)  -- filepath path verifier
        (map pack standardSourceFolders)
  <*> askChoicesWithOther
        "initial project version?"
        0
        (verifyElmVersion . unpack)  -- TODO add verifier
        [pack $ showVersion defaultProjectVersion]
  <*> (putStrLn "a quick summary" >> getLine)
  <*> (putStrLn "project repository url" >> getLine)
  <*> askChoicesWithOther
        "choose a license"
        0
        Just
        availableLicenses
  <*> askChoicesWithOther
        "select the elm-version"
        0
        (Just)  -- add verifier?
        [defaultElmVersion]


mkFiles :: [(FilePath, Maybe ByteString.ByteString)] -> IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile :: FilePath -> Maybe ByteString.ByteString -> IO Result
mkFile name defaultFile = exists name >>=
  bool
    (withFile
      name
      WriteMode
      (flip (maybe (return ())) defaultFile . ByteString.hPut)
    >> return (Right ()))
    (return $ Left $ "file " `append` pack name `append` " already exists")


mkSourceFiles :: FilePath -> IO [Result]
mkSourceFiles =
  mkFiles . flip map standardSourceFiles . Arrow.first . (</>)


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs = mapM_ . (createDirectoryIfMissing True .) . (</>)


writeConf :: FilePath -> UserDecisions -> IO ()
writeConf wd =
  withFile
    (wd </> elmConfigName)
    WriteMode
    . flip LBS.hPut . encodePretty . makePackage


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

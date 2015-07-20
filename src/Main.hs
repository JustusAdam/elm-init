{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where


import           Control.Applicative      ((<$>), (<*>))
import           Control.Arrow            as Arrow (first)
import           Control.Monad            (join)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString          as ByteString (ByteString, hPut)
import qualified Data.ByteString.Char8    as CBS (pack, unpack)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.Char                (isUpper)
import           Data.FileEmbed           (embedFile)
import           Data.Text                as Text (Text, append, pack, unpack)
import qualified Data.Text.IO             as TextIO (getLine, putStrLn)
import           Data.Time
import           Data.Version             (Version (..), makeVersion,
                                           showVersion)
import           ElmInit                  (CmdArgs (..), Result,
                                           UserDecisions (..), askChoices,
                                           askChoicesWithOther, exists,
                                           makePackage, verifyElmVersion)
import           Prelude                  hiding (putStrLn)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory, makeAbsolute)
import           System.Environment       (getArgs)
import           System.FilePath          (isValid, takeBaseName, takeExtension,
                                           (</>))
import           System.IO                (IOMode (WriteMode), withFile)
import           Text.Printf              (printf)


standardDirectories :: [FilePath]
standardDirectories = [ "elm-stuff" ]

standardSourceFolders :: [FilePath]
standardSourceFolders = [ "src" ]

standardFiles :: [(FilePath, Maybe ByteString.ByteString)]
standardFiles = [ ("README.md", Nothing) ]

standardSourceFiles :: [(FilePath, Maybe ByteString.ByteString)]
standardSourceFiles = []

mainFile :: String -> ByteString.ByteString
mainFile = CBS.pack . printf (CBS.unpack $(embedFile "resources/Main.elm"))

indexHtml :: String -> ByteString.ByteString
indexHtml = CBS.pack . printf (CBS.unpack $(embedFile "resources/index.html"))

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
          (TextIO.putStrLn "the chosen directory does not exist yet, shall I create it? [y/N]"
          >> getResp >>=
            bool
              (error "Project directory does not exist")  -- I'm so sorry
              makeDirs
          >> return wd)
          (return wd))
      (error "The chosen directory is a file, you'll have to choose a different name") -- I'm so sorry

  where
    getResp :: IO Bool
    getResp = flip elem ["y", "yes"] <$> TextIO.getLine

    makeDirs = createDirectoryIfMissing True wd


getUserDecisions :: FilePath -> IO UserDecisions
getUserDecisions wd = Default
  <$> askChoicesWithOther
        "project name?"
        0
        return
        [pack $ takeBaseName wd]
  <*> askChoicesWithOther
        "choose a source folder name"
        0
        ((bool (Left "The filepath must be valid") <$> return <*> isValid) . unpack)  -- filepath path verifier
        (map pack standardSourceFolders)
  <*> askChoicesWithOther
        "initial project version?"
        0
        (verifyElmVersion . unpack)
        [pack $ showVersion defaultProjectVersion]
  <*> (TextIO.putStrLn "a quick summary" >> TextIO.getLine)
  <*> (TextIO.putStrLn "project repository url" >> TextIO.getLine)
  <*> askChoicesWithOther
        "choose a license"
        0
        return
        availableLicenses
  <*> askChoicesWithOther
        "select the elm-version"
        0
        return
        [defaultElmVersion]
  <*> askChoicesWithOther
        "What sould be the Main file?"
        0
        (isValidMainFile . unpack)
        ["Main.elm"]
  <*> ((== "Yes") <$> askChoices
        "Should I create an index.html file?"
        0
        ["Yes", "No"]
      )


isValidMainFile :: String -> Either Text String
isValidMainFile file
  | null file = Left "Filename cannot be 𝜖"
  | not $ isUpper (head file) = Left "Module names (and their files) have to start with uppercase"
  | ".elm" /= takeExtension file = Left "Elm modules (such as this main file) have to end with the fileending \".elm\""
  | otherwise = return file


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


mkSourceFiles :: FilePath -> UserDecisions -> IO [Result]
mkSourceFiles wd (Default { mainFileName = mfn, sourceFolder = sourceF, addIndex = makeIndex }) = do
  let mainModule = takeBaseName mfn
  mainFileRes <- mkFile (wd  </> sourceF </> mfn) $ Just $ mainFile mainModule
  indexFileRes <- if makeIndex
    then mkFile (wd </> "index.html") $ Just $ indexHtml mainModule
    else return $ return ()
  others <- mkFiles $ flip map standardSourceFiles $ Arrow.first ((wd </> sourceF) </>)
  return $ mainFileRes:indexFileRes:others


mkDirs :: FilePath -> [FilePath] -> IO ()
mkDirs = mapM_ . (createDirectoryIfMissing True .) . (</>)


writeConf :: FilePath -> UserDecisions -> IO ()
writeConf wd =
  withFile
    (wd </> elmConfigName)
    WriteMode
    . flip LBS.hPut . encodePretty . makePackage


putLicense :: FilePath -> Text -> IO ()
putLicense wd name =
  case join $ lookup name standardLicenses of
    Nothing -> return ()
    Just contents -> do
      TextIO.putStrLn "The project owner(s): (for the license)"
      authors <- getLine
      (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
      writeFile (wd </> "LICENSE") $
        printf (CBS.unpack contents) year $
          if null authors
            then "[project owners]"
            else authors


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
  resSource <- mkSourceFiles wd decisions

  -- write the package config based on the user decisions
  writeConf wd decisions

  -- write the choosen license
  _         <- putLicense wd (license decisions)

  -- report all errors
  mapM_ (either TextIO.putStrLn return) (resStatic ++ resSource)

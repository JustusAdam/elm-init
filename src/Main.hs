{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where


import           Control.Arrow            as Arrow (first)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bool                (bool)
import qualified Data.ByteString          as ByteString (ByteString, hPut)
import qualified Data.ByteString.Lazy     as LBS (hPut)
import           Data.FileEmbed           (embedFile)
import           Data.Text                as Text (Text, append, pack, unpack)
import           Data.Text.IO             as TextIO (getLine, putStrLn)
import           Prelude                  hiding (getLine, putStrLn)
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           getCurrentDirectory, makeAbsolute)
import           System.Environment       (getArgs)
import           System.FilePath          (isValid, takeBaseName, (</>))
import           System.IO                (IOMode (WriteMode), withFile)
import           Data.Version             (Version(..), showVersion,
                                          makeVersion)
import           ElmInit                  (Result, UserDecisions(..),
                                          CmdArgs(..), askChoicesWithOther,
                                          exists, verifyElmVersion, makePackage, flattenMaybe)


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

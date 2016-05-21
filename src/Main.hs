{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main (main) where


import           Control.Applicative         ((<$>), (<*>))
import           Control.Applicative.Unicode
import           Control.Arrow               as Arrow (first)
import           Control.Monad               (join, void, when, unless)
import           Control.Monad.Unicode
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString             as ByteString (ByteString, hPut,
                                                            writeFile)
import qualified Data.ByteString.Char8       as CBS (pack, unpack)
import qualified Data.ByteString.Lazy        as LBS (hPut)
import           Data.Char                   (isUpper)
import           Data.FileEmbed              (embedFile)
import           Data.Monoid.Unicode
import           Data.Text                   as Text (Text, pack, unpack)
import qualified Data.Text.IO                as TextIO (getLine, putStrLn)
import           Data.Time
import           ElmInit.Types               (makeVersion, Version, showVersion)
import           ElmInit                     (CmdArgs (..), Result,
                                              UserDecisions (..), askChoices,
                                              askChoicesWithOther, exists,
                                              makePackage, verifyElmVersion)
import           ElmInit.Util                (bool, withCurrentDirectory)
import           Prelude                     hiding (putStrLn)
import           Prelude.Unicode
import           System.Directory            (createDirectoryIfMissing,
                                              doesDirectoryExist, doesFileExist,
                                              getCurrentDirectory, makeAbsolute)
import           System.Environment          (getArgs)
import           System.FilePath             (isValid, takeBaseName,
                                              takeExtension, (</>), takeDirectory)
import           System.IO                   (IOMode (WriteMode), withFile)
import           System.Process              (callProcess)
import           Text.Printf                 (printf)


standardDirectories ∷ [FilePath]
standardDirectories = [ "elm-stuff" ]

standardSourceFolders ∷ [FilePath]
standardSourceFolders = [ "src" ]

standardFiles ∷ [(FilePath, Maybe ByteString.ByteString)]
standardFiles = [ ("README.md", Nothing) ]

standardSourceFiles ∷ [(FilePath, Maybe ByteString.ByteString)]
standardSourceFiles = []

mainFile ∷ String → ByteString.ByteString
mainFile = CBS.pack ∘ printf (CBS.unpack $(embedFile "resources/Main.elm"))

indexHtml ∷ String → ByteString.ByteString
indexHtml = CBS.pack ∘ printf (CBS.unpack $(embedFile "resources/index.html"))

gitignore ∷ ByteString.ByteString
gitignore = $(embedFile "resources/.gitignore")

standardLicenses ∷ [(Text, Maybe ByteString.ByteString)]
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

defaultProjectVersion ∷ Version
defaultProjectVersion = makeVersion [1, 0, 0]

defaultElmVersion ∷ Text
defaultElmVersion = "0.17.0 <= v < 0.18.0"

availableLicenses ∷ [Text]
availableLicenses = fst $ unzip standardLicenses

elmConfigName ∷ FilePath
elmConfigName = "elm-package.json"


getCmdArgs ∷ IO CmdArgs
getCmdArgs = CmdArgs <$> (getArgs ≫= handleDir)
  where
    handleDir []  = getCurrentDirectory
    handleDir [x] = makeAbsolute x
    handleDir _   = error "Too many arguments"  -- I'm so sorry


verifyWD ∷ FilePath → IO FilePath
verifyWD wd =
  doesFileExist wd ≫=
    bool
      (doesDirectoryExist wd ≫=
        bool
          (TextIO.putStrLn "the chosen directory does not exist yet, shall I create it? [y/N]" >>
          getResp ≫=
            bool
              (error "Project directory does not exist")  -- I'm so sorry
              makeDirs ≫
          return wd)
        (return wd))
      (error "The chosen directory is a file, you'll have to choose a different name") -- I'm so sorry

  where
    getResp ∷ IO Bool
    getResp = (∈ ["y", "yes"]) <$> TextIO.getLine

    makeDirs = createDirectoryIfMissing True wd


getUserDecisions ∷ FilePath → IO UserDecisions
getUserDecisions wd = do
  dir <- makeAbsolute wd
  let basename = takeBaseName dir
      dirname = if null basename then takeBaseName $ takeDirectory dir else basename
  Default
    <$> askChoicesWithOther
          "project name?"
          0
          return
          [pack dirname]
    ⊛ askChoicesWithOther
          "choose a source folder name"
          0
          ((bool (Left "The filepath must be valid") <$> return ⊛ isValid) ∘ unpack)  -- filepath path verifier
          (map pack standardSourceFolders)
    ⊛ askChoicesWithOther
          "initial project version?"
          0
          (verifyElmVersion ∘ unpack)
          [pack $ showVersion defaultProjectVersion]
    ⊛ (TextIO.putStrLn "a quick summary" ≫ TextIO.getLine)
    ⊛ (TextIO.putStrLn "project repository url" ≫ TextIO.getLine)
    ⊛ askChoicesWithOther
          "choose a license"
          0
          return
          availableLicenses
    ⊛ askChoicesWithOther
          "select the elm-version"
          0
          return
          [defaultElmVersion]
    ⊛ askChoicesWithOther
          "What sould be the Main file?"
          0
          (isValidMainFile . unpack)
          ["Main.elm"]
    ⊛ ((≡ "Yes") <$> askChoices
          "Should I create an index.html file?"
          0
          ["Yes", "No"]
        )


isValidMainFile ∷ String → Either Text String
isValidMainFile file
  | null file = Left "Filename cannot be 𝜖"
  | not $ isUpper (head file) =
    Left "Module names (and their files) have to start with uppercase"
  | ".elm" ≢ takeExtension file =
    Left "Elm modules (such as this main file) have to end with the fileending \".elm\""
  | otherwise = return file


mkFiles ∷ [(FilePath, Maybe ByteString.ByteString)] → IO [Result]
mkFiles = mapM (uncurry mkFile)


mkFile ∷ FilePath → Maybe ByteString.ByteString → IO Result
mkFile name defaultFile = exists name ≫=
  bool
    (withFile
      name
      WriteMode
      (flip (maybe (return ())) defaultFile ∘ ByteString.hPut) ≫
    return (Right ()))
    (return $ Left $ "file " ⊕ pack name ⊕ " already exists")


mkSourceFiles ∷ UserDecisions → IO [Result]
mkSourceFiles (Default { mainFileName = mfn, sourceFolder = sourceF, addIndex = makeIndex }) = do
  let mainModule = takeBaseName mfn
  mainFileRes ← mkFile (sourceF </> mfn) $ Just $ mainFile mainModule
  indexFileRes ← if makeIndex
    then mkFile "index.html" $ Just $ indexHtml mainModule
    else return $ return ()
  others ← mkFiles standardSourceFiles
  return $ mainFileRes:indexFileRes:others


mkDirs ∷ [FilePath] → IO ()
mkDirs = mapM_ (createDirectoryIfMissing True)


writeConf ∷ UserDecisions → IO ()
writeConf =
  withFile
    elmConfigName
    WriteMode
    ∘ flip LBS.hPut ∘ encodePretty ∘ makePackage


putLicense ∷ Text → IO ()
putLicense name =
  case join $ lookup name standardLicenses of
    Nothing → return ()
    Just contents → do
      TextIO.putStrLn "The project owner(s): (for the license)"
      authors ← getLine
      (year, _, _) ← toGregorian ∘ utctDay <$> getCurrentTime
      writeFile "LICENSE" $
        printf (CBS.unpack contents) year $
          if null authors
            then "[project owners]"
            else authors


initGit ∷ IO ()
initGit = do
  hasGit <- doesDirectoryExist ".git"
  unless hasGit $ do
    TextIO.putStrLn "Shall I initialize a git repository? [Y/n]"
    answ ← getLine
    when (answ `elem` ["y", "Y", ""]) $ do
      ByteString.writeFile ".gitignore" gitignore
      void $ callProcess "git" ["init"]


main ∷ IO ()
main = do

  -- get either the working directory or the directory the user entered
  wd        ← getCmdArgs ≫= (verifyWD ∘ workingDirectory)

  withCurrentDirectory wd $ do

    -- ask all important input first
    decisions ← getUserDecisions wd

    -- create necessary directories
    mkDirs (sourceFolder decisions : standardDirectories)

    -- create non-dynamic files, collect errors
    resStatic ← mkFiles standardFiles

    -- create Elm source files, collect errors
    resSource ← mkSourceFiles decisions

    -- write the package config based on the user decisions
    writeConf decisions

    -- write the choosen license
    _         ← putLicense (license decisions)

    initGit

    -- report all errors
    mapM_ (either TextIO.putStrLn return) (resStatic ⧺ resSource)

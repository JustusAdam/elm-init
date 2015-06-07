{-# LANGUAGE OverloadedStrings #-}

module ElmInit.Types
  ( Result
  , CmdArgs(..)
  , UserDecisions(..)
  , ElmPackage(..)
  , readVersion
  , makePackage
  , verifyElmVersion
  , readOneVersion
  ) where


import           Data.Aeson                   as Aeson (ToJSON, object, (.=),
                                              toJSON, Value)
import           Control.Applicative          ((<*>), (<$>))
import           Control.Monad                ((<=<))
import           Data.Version                 (Version(Version), makeVersion,
                                              showVersion, parseVersion)
import           Data.Text                    (pack, Text)
import           Text.ParserCombinators.ReadP (readP_to_S)



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


readOneVersion :: String -> Maybe Version
readOneVersion = verif . readVersion
  where
    verif ((v, []):_) = Just v
    verif []          = Nothing
    verif (_:xs)      = verif xs

verifyElmVersion :: String -> Maybe Version
verifyElmVersion = hasElmStructure <=< readOneVersion
  where
    hasElmStructure v@(Version [ _, _, _ ] []) = Just v
    hasElmStructure _                          = Nothing

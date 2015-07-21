{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

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


import           Control.Applicative
import           Control.Applicative.Unicode
import           Control.Monad                ((<=<))
import           Data.Aeson                   as Aeson (ToJSON, Value, object,
                                                        toJSON, (.=))
import           Data.Text                    (Text, pack)
import           Data.Version                 (Version (Version), makeVersion,
                                               parseVersion, showVersion)
import           Prelude.Unicode
import           Text.ParserCombinators.ReadP (readP_to_S)


type Result = Either Text ()


data CmdArgs = CmdArgs { workingDirectory ∷ FilePath }


data UserDecisions = Default { projectName  ∷ Text
                             , sourceFolder ∷ FilePath
                             , version      ∷ Version
                             , summary      ∷ Text
                             , repository   ∷ Text
                             , license      ∷ Text
                             , elmVersion   ∷ Text
                             , mainFileName ∷ String
                             , addIndex     ∷ Bool
                             }


data ElmPackage = ElmPackage { pkgVersion        ∷ Version
                             , pkgSummary        ∷ Text
                             , pkgRepository     ∷ Text
                             , pkgLicense        ∷ Text
                             , pkgDependencies   ∷ Aeson.Value
                             , pkgExposedModules ∷ [Text]
                             , pkgElmVersion     ∷ Text
                             , pkgSourceDirs     ∷ [Text]
                             }


readVersion ∷ String → [(Version, String)]
readVersion = readP_to_S parseVersion


instance Aeson.ToJSON ElmPackage where
  toJSON = object . sequenceA
    [ ("version" .=)          ∘ showVersion ∘ pkgVersion
    , ("summary" .=)          ∘ pkgSummary
    , ("repository" .=)       ∘ pkgRepository
    , ("license" .=)          ∘ pkgLicense
    , ("dependencies" .=)     ∘ pkgDependencies
    , ("exposed-modules" .=)  ∘ pkgExposedModules
    , ("elm-version" .=)      ∘ pkgElmVersion
    , ("source-directories".=)∘ pkgSourceDirs
    ]


emptyDecisions ∷ UserDecisions
emptyDecisions =
  Default { summary       = ""
          , repository    = ""
          , version       = makeVersion [0, 0, 0]
          , license       = ""
          , sourceFolder  = ""
          , projectName   = ""
          , elmVersion    = ""
          , mainFileName  = "Main.elm"
          , addIndex      = True
          }


makePackage ∷ UserDecisions → ElmPackage
makePackage = ElmPackage
  <$> version
  ⊛ summary
  ⊛ repository
  ⊛ license
  ⊛ const (object [("elm-lang/core", "2.0.0 <= v < 3.0.0")])
  ⊛ const []
  ⊛ elmVersion
  ⊛ (:[]) . pack . sourceFolder


readOneVersion ∷ String → Either Text Version
readOneVersion = verif ∘ readVersion
  where
    verif ((v, []):_) = return v
    verif []          = Left "Version must have this structure: 1.2.3"
    verif (_:xs)      = verif xs


verifyElmVersion ∷ String → Either Text Version
verifyElmVersion = hasElmStructure <=< readOneVersion
  where
    hasElmStructure v@(Version [ _, _, _ ] []) = Right v
    hasElmStructure _                          = Left "Version must have this structure: 1.2.3"

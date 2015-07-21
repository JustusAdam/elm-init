{-# LANGUAGE UnicodeSyntax #-}
module ElmInit.Util
  ( exists
  , getOr
  , enumerate
  ) where


import           Control.Applicative.Unicode
import           Control.Exception           (IOException, catch)
import           Control.Monad.Unicode
import           Data.Bool                   (bool)
import           Prelude.Unicode
import           System.Directory            (doesDirectoryExist, doesFileExist)


exists ∷ FilePath → IO Bool
exists =
  (>>=)
  <$> doesFileExist
  ⊛ (flip bool
        (return True)
        ∘ doesDirectoryExist)


getOr ∷ Read a ⇒ a → IO a
getOr =
  catch readLn ∘ handler
  where
    handler ∷ a → IOException → IO a
    handler = const ∘ return


enumerate ∷ Int → [a] → [(Int,a)]
enumerate from l = zip [from..(length l)] l

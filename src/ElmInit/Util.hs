{-# LANGUAGE UnicodeSyntax #-}
module ElmInit.Util
  ( exists
  , getOr
  , enumerate
  , bool
  ) where


import           Control.Applicative         ((<$>))
import           Control.Applicative.Unicode
import           Control.Exception           (IOException, catch)
import           Prelude.Unicode
import           System.Directory            (doesDirectoryExist, doesFileExist)


bool :: a -> a -> Bool -> a
bool a _ False = a
bool _ a True = a


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

{-# LANGUAGE OverloadedStrings #-}

module ElmInit.Interact
  ( askChoicesWithOther
  , askChoices
  ) where


import qualified Control.Arrow            as Arrow (first)
import           Control.Applicative      ((<$>), (<*>))
import           Data.Text                (append)
import           Data.Bool                (bool)
import           ElmInit.Util             (getOr, enumerate)
import           Data.Text                as Text (Text, intercalate, pack)
import           Data.Text.IO             as TextIO (getLine, putStrLn)
import           Prelude                  hiding (getLine, putStrLn)


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

    ask a =
      putStrLn a >>
      getOr selected >>=
        (bool
          (putStrLn "invalid choice, please choose again" >>
          ask a)
          <$> return
          <*> (<= length choices))


askChoicesWithOther :: Text -> Int -> (Text -> Either Text a) -> [Text] -> IO a
askChoicesWithOther m s trans l =
    askChoices' m s (l ++ ["other (specify)"])
    >>= (flip bool getAlternative
          <$> either (const $ error "No parse") return . trans . (l !!)
          <*> (== length l))

  where
    getAlternative =
      putStrLn "please enter an alternative" >>
      getLine >>=
        (either
          (\message ->
            putStrLn "Invalid input:" >>
            putStrLn message >>
            getAlternative
          )
          return
          . trans)

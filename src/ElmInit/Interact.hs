{-# LANGUAGE OverloadedStrings #-}

module ElmInit.Interact
  ( askChoicesWithOther
  ) where


import qualified Control.Arrow            as Arrow (first)
import           Data.Text                (append)
import           Data.Bool                (bool)
import           ElmInit.Util             (getOr)
import           Data.Text                as Text (Text, intercalate, pack)
import           ElmInit.Util             (enumerate)
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

    ask =
          (>>)
          <$> putStrLn
          <*> (\a ->
                getOr selected >>=
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

module Polysemy.Utils
  ( module Polysemy.Utils
  , QuasiQuoter
  ) where

import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
import           Polysemy
import Data.List

type InterpreterOf r e = ∀ a. Semantic (e ': r) a -> Semantic r a


sendQQ :: TH.Name -> QuasiQuoter -> QuasiQuoter
sendQQ n qq = QuasiQuoter
  { quoteExp = \str -> do
      z <- quoteExp qq str
      pure $ TH.AppE (TH.VarE n) z
  , quotePat = error "bad qq"
  , quoteType = error "bad qq"
  , quoteDec = error "bad qq"
  }

paragraphs :: QuasiQuoter
paragraphs = QuasiQuoter
  { quoteExp = \str -> do
      let ls = textToParagraphs str
      pure $ TH.ListE $ fmap (TH.LitE . TH.StringL) ls
  , quotePat = error "bad qq"
  , quoteType = error "bad qq"
  , quoteDec = error "bad qq"
  }

textToParagraphs :: String -> [String]
textToParagraphs str =
  let ls = dropWhile ((== "") . dropWhile (== ' ')) $ lines str
      numSpaces = length $ takeWhile (== ' ') $ head ls
   in fmap (intercalate " " . dropWhile (== ""))
    . groupBy (const $ (/= ""))
    . reverse
    . dropWhile (== "")
    . reverse
    $ fmap (drop numSpaces) ls


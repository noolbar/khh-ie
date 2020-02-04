module Text.Marked.Types where

import Prelude

import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, readImpl)

newtype Markdown = Markdown String
derive instance newtyepMarkdown ∷ Newtype Markdown _
instance readForeignMarkdown ∷ ReadForeign Markdown where
  readImpl o = do
    str ← readImpl o
    pure $ Markdown str

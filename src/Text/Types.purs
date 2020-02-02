module Text.Marked.Types where

import Data.Newtype (class Newtype)

newtype Markdown = Markdown String

derive instance newtyepMarkdown ∷ Newtype Markdown _

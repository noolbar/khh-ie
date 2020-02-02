module Text.Marked.Impl where


import Data.Function.Uncurried (Fn1, runFn1)
import Text.Marked.Types (Markdown)

foreign import _marked ∷ Fn1 String Markdown

marked ∷ String → Markdown
marked a = runFn1 _marked a

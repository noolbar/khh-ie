module Text.Marked.Impl where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Newtype (unwrap)
import Effect (Effect)
import Text.Marked.Types (Markdown)
import Web.DOM (Document)
import Web.DOM.Document as Doc
import Web.DOM.Element as Elem
import Web.DOM.Node as Node

foreign import _marked ∷ Fn1 String (Effect String)
foreign import _outerHTML ∷ Fn2 String Node.Node (Effect Unit)

appendMarkedNode ∷ Markdown → Document → Node.Node → Effect Unit
appendMarkedNode a document node = do
  str ← runFn1 _marked (unwrap a)
  void $ Doc.createElement "a" document >>= \elem → do
    let e = Elem.toNode elem
    Node.appendChild e node >>= runFn2 _outerHTML str


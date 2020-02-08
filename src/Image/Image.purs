module Image.Impl where

import Prelude

import Data.Newtype (unwrap)
import Effect (Effect)
import KKH.Types (Base64)
import Web.DOM (Document)
import Web.DOM.Document as Doc
import Web.DOM.Element as Elem
import Web.DOM.Node as Node

createImagedNode ∷ String → Base64 → Document → Effect Node.Node
createImagedNode imageType a document = do
  Doc.createElement "img" document >>= \elem → do
    Elem.setAttribute "src" ("data:image/" <> imageType <> ";base64," <> (unwrap a)) elem
    pure $ Elem.toNode elem

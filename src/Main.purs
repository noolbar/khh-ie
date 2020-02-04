module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (traverse1_)
import Data.String.CodeUnits as Str
import Data.Traversable (foldr, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Effect.Console as Console
import KKH.Types (Cell(..), Cell'(..), CellOutput(..), CellOutput'(..), Link, Note(..), Showable(..))
import Simple.JSON as JSON
import Text.Marked.Impl (appendMarkedNode)
import Text.Marked.Types (Markdown(..))
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element as Elem
import Web.DOM.Node as Node
import Web.Event.EventTarget as DOM
import Web.HTML as HTML
import Web.HTML.Event.HashChangeEvent.EventTypes as HCHE
import Web.HTML.HTMLDocument as HDoc
import Web.HTML.HTMLElement as HElem
import Web.HTML.Location as Location
import Web.HTML.Window as Window

main ∷ Effect Unit
main = do
  htmlDocument ← HTML.window >>= Window.document

  maybeBody ← HDoc.body htmlDocument
  let document = HDoc.toDocument htmlDocument

  url ← HTML.window >>= Window.location >>= Location.href
  hash ← HTML.window >>= Window.location >>= Location.hash >>= (\hs → pure $ Str.drop 1 hs)

  listener ← DOM.eventListener $ const (HTML.window >>= Window.location >>= Location.reload)
  HTML.window >>= Window.toEventTarget >>> DOM.addEventListener HCHE.hashchange listener false

  launchAff_ $ do
    result ← AX.request (AX.defaultRequest  { -- url = "/sample-data/1234abc.json"
                                              url = hash
                                            , method = Left GET
                                            , responseFormat = ResponseFormat.string
                                            })
    liftEffect case maybeBody of
      Nothing → Console.error "body is not found"
      Just body → do
        pNode ← Elem.toNode <$> createElement "h2" document
        Node.appendChild pNode (HElem.toNode body) >>=
          Node.setTextContent ("KHHnote for IE" <> ": " <> hash)

        case result of
          Left err → log $ "GET note response failed to decode: " <> AX.printError err
          Right response → do
            log $ "GET note response: " <> response.body
            case JSON.readJSON response.body of
              Right (Note note) → do
                log $ "GET note response: " <> (unwrap note.key)
                mkContainer (Note note) document body
              Left err → traverse1_ logShow err

  where
    mkContainer ∷ Note → Document → HElem.HTMLElement → Effect Unit
    mkContainer (Note note) document body = do
      let node = HElem.toNode body
      contmemu ← createElement "div" document >>= \elem → do
        Elem.setAttribute "style" "width: 200px; float: left;" elem
        pure $ Elem.toNode elem

      void $ Node.appendChild contmemu node >>= \memu → do
        void $ Elem.toNode <$> createElement "div" document >>= \div → do
          Node.appendChild div memu >>= \elem → do
            Node.setTextContent "Directories" elem
            traverse (\l → addLinks l document elem) note.directories
        void $ Elem.toNode <$> createElement "div" document >>= \div → do
          Node.appendChild div memu >>= \elem → do
            Node.setTextContent "Dependencies" elem
            traverse (\l → addLinks l document elem) note.dependencies
      void $ createElement "div" document >>= \elem → do
        Elem.setAttribute "style" "width: 80vw; float: left;" elem
        let div = Elem.toNode elem
        Node.appendChild div node >>= \cellcont → do
          void $ traverse (\c → render c document cellcont) note.cells

    render ∷ Cell → Document → Node.Node → Effect Unit
    render (Cell cell) document node = do
      celldiv ← createElement "div" document >>= \elem → do
                  Elem.setAttribute "style" "border: solid; word-break: break-all;" elem
                  pure $ Elem.toNode elem
      Node.appendChild celldiv node >>=
        appendCell cell
      where
        appendCell ∷ Cell' → Node.Node → Effect Unit
        appendCell (PlainTextCell r) cellbody = do
          sNode ← wall r "source"
          Node.setTextContent (r.source) sNode
          void $ Node.appendChild sNode cellbody
        appendCell (CodeCell r) cellbody = do
          sNode ← wall r "source"
          Node.setTextContent ("source: " <> r.source) sNode
          oNode ← wall r "output"
          Node.setTextContent ("output: " <> foldr (<>) "" (map dt r.outputs)) oNode
          void $ Node.appendChild sNode cellbody
          void $ Node.appendChild oNode cellbody
        appendCell (MarkDownCell r) cellbody = do
          void $ appendMarkedNode (Markdown r.source) document cellbody
        appendCell (ImagePngCell r) cellbody = appendCell (PlainTextCell r) cellbody

        appendCell (ImageJpegCell r) cellbody = appendCell (PlainTextCell r) cellbody

        wall r str = createElement "PlainText" document >>= \elem → do
          Elem.setAttribute "id" (unwrap r.key) elem
          Elem.setAttribute "style" (showattr str r.showable) elem
          pure $ Elem.toNode elem

        getOutput (PlainTextOut r) = r
        getOutput (MarkDownOut r) = r
        getOutput (ImagePngOut r) = unwrap r
        getOutput (ImageJpegOut r) = unwrap r
        dt :: CellOutput → String
        dt (CellOutput output) = getOutput output

    showattr ∷ String → Showable → String
    showattr str able = case str, able of
      _, None → "display: none;"
      "output", Source → "display: none;"
      "source", Output → "display: none;"
      _, _ → "display: block;"

    addLinks ∷ Link → Document → Node.Node → Effect Unit
    addLinks link document node = do
      ref ← createElement "a" document >>= \elem → do
            Elem.setAttribute "href" (unwrap link.url) elem
            Node.setTextContent link.title $ Elem.toNode elem
            pure $ Elem.toNode elem
      div ← Elem.toNode <$> createElement "div" document
      void $ Node.appendChild div node >>=
              Node.appendChild ref

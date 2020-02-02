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
import KKH.Types (Cell(..), Cell'(..), CellOutput(..), CellOutput'(..), Note(..))
import Simple.JSON as JSON
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element as Elem
import Web.DOM.Node as Node
import Web.HTML as HTML
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
  
  launchAff_ $ do
    result ← AX.request (AX.defaultRequest  { -- url = "/sample-data/1234abc.json"
                                              url = hash
                                            , method = Left GET
                                            , responseFormat = ResponseFormat.string
                                            })
    liftEffect case maybeBody of 
      Nothing -> Console.error "body is not found"
      Just body -> do
        pNode <- Elem.toNode <$> createElement "p" document
        Node.appendChild pNode (HElem.toNode body) >>=
          Node.setTextContent ("KHH for IE" <> " current url: " <> url <> " hash: " <> hash)

        case result of
          Left err -> log $ "GET /sample-data response failed to decode: " <> AX.printError err
          Right response -> do
            log $ "GET /sample-data response: " <> response.body
            case JSON.readJSON response.body of
              Right (Note {key, cells}) -> do
                log $ "GET /sample-data response: " <> (unwrap key)
                void $ traverse (\c → render c document body) cells
              Left err → traverse1_ logShow err
              
  where
    render ∷ Cell → Document → HElem.HTMLElement → Effect Unit 
    render (Cell cell) document body= do
      let getCell (PlainTextCell r) = r
          getCell (CodeCell r) = r 
          getCell (MarkDownCell r) = r 
          getCell (ImagePngCell r) = r 
          getCell (ImageJpegCell r) = r 
          cr = getCell cell
          getOutput (PlainTextOut r) = r
          getOutput (MarkDownOut r) = r
          getOutput (ImagePngOut r) = unwrap r
          getOutput (ImageJpegOut r) = unwrap r
          dt :: CellOutput → String
          dt output =
            let CellOutput outputs = output
            in getOutput outputs
      pNode ← Elem.toNode <$> createElement "p" document
      Node.appendChild pNode (HElem.toNode body) >>=
        Node.setTextContent ("hash: " <> unwrap cr.key <> " source: " <> cr.source <> " output: " <> foldr (<>) "" (map dt cr.outputs))

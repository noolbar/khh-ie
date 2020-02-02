module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import KKH.Types (MD5(..))
import Pages.Home as Home
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (Location, window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Location as Location
import Web.HTML.Window as Window


-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener <- DOM.eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: (forall a. Home.Query a -> Aff (Maybe a))
  -> CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  void $ query $ H.tell $ Home.ChangeRoute hash
  pure Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  url ← liftEffect $ DOM.window >>= Window.location >>= Location.origin
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ url
  let input = Home.default { url = Home.URL url
                           , hash = MD5 hash
                           }
  io <- runUI Home.component input body

  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)

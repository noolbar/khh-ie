module Button where
  
import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

component ∷ ∀ q i o m. H.Component HH.HTML q i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
  }

type State = { enabled ∷ Boolean }
data Action = Toggle

initialState ∷ ∀ i. i → State
initialState _ = { enabled: false }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render st =
  let
    label = if st.enabled then "ON" else "OFF"
  in
    HH.button
      [ HP.title label
      , HE.onClick \_ → Just Toggle
      ]
      [ HH.text label ]

handleAction ∷ ∀ o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st → st { enabled = not st.enabled }

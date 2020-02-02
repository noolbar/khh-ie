module Pages.Home where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Html.Renderer.Halogen as RH
import KKH.Types (Cell(..), MD5(..), Note, Output(..), Showable(..))
import Text.Marked.Impl (marked)

newtype URL = URL String
derive instance newtypeURL ∷ Newtype URL _

type State = { url ∷ URL
             , entryPoint ∷ URL
             , hash ∷ MD5
             , note ∷ Note
             }

data Action = Load

data Query a = ChangeRoute String a

component ∷ ∀ o m. H.Component HH.HTML Query State o m
component = H.mkComponent
  { initialState
  , render 
  , eval : H.mkEval $ H.defaultEval { handleAction = handleAction
                                    , handleQuery = handleQuery
                                    }
  }

default ∷ State
default =
  { url : URL "example.com"
  , entryPoint : URL "localhost"
  , hash : MD5 "123456"
  , note :
    { key : MD5 "123"
    , cells :
        [ PlainTextCell
          { metadata :  ""
          , key : MD5 "456"
          , showable : SourceOutput
          , source : "default Source"
          , outputs :
              [  PlainTextOut "default Output"
              ]
          }
        ]
    }
  }

initialState ∷ ∀ i. i → State
initialState _ = default


render ∷ ∀ a m. State → H.ComponentHTML a () m
render st = 
  HH.div_
      [ RH.render_ $ unwrap $ marked "test"
      , HH.text $ (unwrap st.url) <> " hash:" <> (unwrap st.hash)
      ]

  where
      _button :: SProxy "button"
      _button = SProxy

handleAction ∷ ∀ o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of 
  Load → H.modify_ $ \st → st { url = URL "example.coms" } 

handleQuery ∷ ∀ o m a. Query a → H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a → do
    H.modify_ \st → st { url = URL msg }
    pure (Just a)

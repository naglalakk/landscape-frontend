module Page.Admin.Home where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Halogen                      as H
import Halogen.HTML                 as HH

import Component.Utils              (OpaqueSlot)
import Component.Table              as TableComponent
import Component.HTML.Admin         (withAdmin)
import Component.HTML.Utils         (css)
import Data.BlogPost                (BlogPost(..)
                                    ,BlogPostArray)
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPosts)
import Timestamp                    (formatToDateStr)

type State = {}

data Action 
  = Initialize

type ChildSlots = ()

initialState :: State
initialState = {}

component :: forall m
           . H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ withAdmin $
        HH.div
          []
          [ HH.text "Testing" ]
      ]

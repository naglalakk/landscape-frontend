module Page.Admin.Exhibitions where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.HTML.Utils (css, safeHref)
import Component.Table as Table
import Data.Array (filter)
import Data.Const (Const(..))
import Data.Exhibition (Exhibition(..), ExhibitionId(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Route as R
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Exhibition (class ManageExhibition, allExhibitions, deleteExhibition)

type State = 
  { exhibitions :: Array Exhibition
  }

data Action 
  = Initialize
  | HandleTableAction Table.Output

type ChildSlots = (
  table :: H.Slot (Const Void) Table.Output Unit
)

component :: forall q i o m
           . MonadEffect m
          => ManageExhibition m
          => Navigate m
          => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: State
  initialState = 
    { exhibitions: []
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      exhibitions <- allExhibitions
      H.modify_ _ { exhibitions = exhibitions }
    HandleTableAction act -> case act of
      Table.UpdateRow row -> do
        navigate $ R.AdminExhibition $ ExhibitionId row.id

      Table.DeleteRow row -> do
        _ <- deleteExhibition $ ExhibitionId row.id
        state <- H.get
        let 
          exs = 
            filter
              (\(Exhibition e) -> e.id == ExhibitionId row.id)
              state.exhibitions
        H.modify_ _ { exhibitions = exs }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div
      []
      [ HH.h1
        []
        [ HH.text "Exhibitions" ]

      , HH.a
        [ safeHref $ R.AdminExhibition $ ExhibitionId 0
        , css "button" 
        ]
        [ HH.text "+ New Exhibition" ]
      , HH.slot
        (SProxy :: _ "table")
        unit
        (Table.component)
        { rows: map (\(Exhibition ex) -> { id: unwrap ex.id, label: ex.title }) state.exhibitions
        , headers: ["Title", "Update", "Delete" ]
        }
      (Just <<< HandleTableAction)
      ]

module Page.Admin.Exhibition where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Data.Const (Const(..))
import Data.Exhibition (Exhibition(..), ExhibitionId(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.Route as R
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Form.Admin.ExhibitionForm as ExhibitionForm
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Exhibition (class ManageExhibition, createExhibition, getExhibitionById, updateExhibition)
import Resource.Media (class ManageMedia)
import Resource.Item (class ManageItem, createItem, updateItem)
import Resource.Token (class ManageToken)

type Input =
  { exhibitionId :: ExhibitionId
  }

type State = 
  { exhibitionId :: ExhibitionId
  , exhibition :: Maybe Exhibition
  }

type ChildSlots = (
  formless :: H.Slot (F.Query ExhibitionForm.ExhibitionForm (Const Void) ExhibitionForm.ChildSlots) Exhibition Unit
)

data Action 
  = Initialize
  | HandleExhibitionForm Exhibition

component :: forall q o m
           . MonadEffect m
          => MonadAff m
          => ManageExhibition m
          => ManageItem m
          => ManageMedia m
          => ManageToken m
          => Navigate m
          => H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just Initialize
      }
    }
  where
  initialState :: Input -> State
  initialState inp = 
    { exhibitionId: inp.exhibitionId
    , exhibition: Nothing
    }
  
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      case state.exhibitionId of
        ExhibitionId 0 -> pure unit
        _ -> do
          exhibition <- getExhibitionById state.exhibitionId
          H.modify_ _ { exhibition = exhibition }

    HandleExhibitionForm (Exhibition exhibition) -> do
      if exhibition.id == (ExhibitionId 0)
        then do
          newExhibition <- createExhibition $ Exhibition exhibition
          case newExhibition of
            Just (Exhibition ex) -> 
              navigate $ R.AdminExhibition ex.id
            Nothing -> pure unit
        else do
          updated <- updateExhibition $ Exhibition exhibition
          H.modify_ _ { exhibition = updated }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div_ 
      [ HH.h1
        []
        [ HH.text "Exhibition" ]
      , HH.slot
        (SProxy :: _ "formless")
        unit
        (ExhibitionForm.component)
        { exhibition: state.exhibition }
        (Just <<< HandleExhibitionForm)
      ]

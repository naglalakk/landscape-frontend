module Form.Admin.ExhibitionForm where

import Prelude

import Component.HTML.Utils (css, withLabel)
import Component.Table as Table
import Data.Array (filter, head, snoc)
import Data.Const (Const(..))
import Data.Exhibition (ExhibitionId(..), Exhibition(..))
import Data.Image (Image(..))
import Data.Item (Item(..), ItemId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Form.Admin.ItemForm as ItemForm
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Resource.Exhibition (class ManageExhibition, getExhibitionItems)
import Resource.Item (class ManageItem, createItem, updateItem)
import Resource.Media (class ManageMedia)
import Resource.Token (class ManageToken)
import Timestamp (Timestamp)

newtype ExhibitionForm r f = ExhibitionForm (r
  ( id :: f Void ExhibitionId ExhibitionId
  , title :: f Void String String
  , featuredImage :: f Void (Maybe Image) (Maybe Image)
  , introduction :: f Void (Maybe String) (Maybe String)
  , items :: f Void (Array ItemId) (Array ItemId)
  , startDate :: f Void Timestamp Timestamp
  , endDate :: f Void Timestamp Timestamp
  , createdAt :: f Void Timestamp Timestamp
  , updatedAt :: f Void (Maybe Timestamp) (Maybe Timestamp)
  ))

derive instance newtypeExhibitionForm :: Newtype (ExhibitionForm r f) _

prx :: F.SProxies ExhibitionForm
prx = F.mkSProxies (F.FormProxy :: F.FormProxy ExhibitionForm)

type Input = 
  { exhibition :: Maybe Exhibition
  }

data Action
  = Initialize
  | Receive Input
  | HandleItemForm Item
  | HandleTableAction Table.Output
  | SaveExhibition

type ChildSlots = (
  itemForm :: H.Slot (F.Query ItemForm.ItemForm (Const Void) ItemForm.ChildSlots) Item Unit,
  itemTable :: H.Slot (Const Void) Table.Output Unit
)

type Query = Const Void

type AddedState = (
  item :: Maybe Item,
  items :: Array Item
)

component :: forall m
        . MonadAff m
      => MonadEffect m
      => ManageExhibition m
      => ManageItem m
      => ManageMedia m
      => ManageToken m
      => F.Component ExhibitionForm Query ChildSlots Input Exhibition m
component = F.component input F.defaultSpec
  { render = render
  , handleEvent = handleEvent
  , handleAction = handleAction
  , initialize = Just Initialize
  , receive = Just <<< Receive
  }
  where
  input inp =
    { initialInputs: case inp.exhibition of
      Just (Exhibition ex) ->
        Just $ F.wrapInputFields ex
      Nothing -> Nothing
    , validators: ExhibitionForm
      { id: F.noValidation
      , title: F.noValidation
      , featuredImage: F.noValidation
      , introduction: F.noValidation
      , items: F.noValidation
      , startDate: F.noValidation
      , endDate: F.noValidation
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    , item: Nothing
    , items: []
    }

  handleAction :: Action
               -> F.HalogenM ExhibitionForm AddedState Action ChildSlots Exhibition m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      let id = F.getInput prx.id state.form
      items <- getExhibitionItems id
      H.modify_ _ { items = items }
    
    HandleItemForm (Item item) -> do
      state <- H.get
      if item.id == ItemId 0
        then do
          itm <- createItem $ Item item
          case itm of
            Just (Item im) -> do
              st <- H.get
              eval $ F.setValidate prx.items (snoc (F.getInput prx.items st.form) im.id)
              H.modify_ _ { items = snoc state.items (Item im) }
            Nothing -> pure unit
        else do 
          _ <- updateItem $ Item item
          pure unit

    HandleTableAction act -> case act of
      Table.UpdateRow row -> do
        state <- H.get
        let 
          hit = head $ filter (\(Item item) -> item.id == (ItemId row.id)) state.items
        H.modify_ _ { item = hit }

      Table.DeleteRow row -> pure unit

    Receive inp -> do
      case inp.exhibition of
        Just (Exhibition ex) -> do
          eval $ F.setValidate prx.id ex.id
          eval $ F.setValidate prx.title ex.title
          eval $ F.setValidate prx.featuredImage ex.featuredImage
          eval $ F.setValidate prx.introduction ex.introduction
          eval $ F.setValidate prx.items ex.items
          eval $ F.setValidate prx.startDate ex.startDate
          eval $ F.setValidate prx.endDate ex.endDate
          eval $ F.setValidate prx.createdAt ex.createdAt
          eval $ F.setValidate prx.updatedAt ex.updatedAt

          items <- getExhibitionItems ex.id
          H.modify_ _ { items = items }
        Nothing -> pure unit

    SaveExhibition -> do
      pure unit
      -- Query for Item form
      -- F.sendQuery F._formless unit (SProxy :: _ "itemForm") 
      eval F.submit

    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event ExhibitionForm AddedState
              -> F.HalogenM ExhibitionForm AddedState Action ChildSlots Exhibition m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      let 
        fields = F.unwrapOutputFields form
      H.raise $ Exhibition fields
    _ -> pure unit

  render :: F.PublicState ExhibitionForm AddedState
         -> F.ComponentHTML ExhibitionForm Action ChildSlots m
  render st =
    HH.div_ 
      [ withLabel "Title" (HH.input
        [ css "text-input"
        , HP.value $ F.getInput prx.title st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.title
        ])
      , withLabel "Introduction" (HH.textarea
        [ HP.value $ fromMaybe "" $ F.getInput prx.introduction st.form
        , HE.onValueInput (\str -> Just $ F.setValidate prx.introduction (Just str))
        , HP.rows 10
        , HP.cols 60
        ])
      , withLabel "Items" (HH.slot
        (SProxy :: _ "itemTable")
        unit
        (Table.component)
        { rows: map (\(Item item) -> 
          { id: unwrap item.id
          , label: item.title
          }) st.items
        , headers: ["Title", "Update", "Delete" ]
        }
        (Just <<< F.injAction <<< HandleTableAction))
      , HH.slot
        (SProxy :: _ "itemForm")
        unit
        (ItemForm.component)
        { item: st.item }
        (Just <<< F.injAction <<< HandleItemForm)
      , HH.button
        [ css "button" 
        , HE.onClick \_ -> Just $ F.injAction SaveExhibition
        ]
        [ HH.text "Save" ]
      ]

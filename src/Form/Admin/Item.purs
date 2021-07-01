module Form.Admin.ItemForm where

import Prelude

import Component.Dropdown as Dropdown
import Component.HTML.Utils (css, maybeElem, withLabel)
import Component.Media as Media
import Data.Array (find, head)
import Data.Const (Const(..))
import Data.Image (Image(..), ImageType)
import Data.Item (Item(..), ItemId(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Token (Token(..), TokenId(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Form.Admin.TokenForm as TokenForm
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Media.Component.Browser as Browser
import Halogen.Media.Data.Media (Media(..))
import Resource.Media (class ManageMedia)
import Resource.Token (class ManageToken, allTokens)
import Timestamp (Timestamp(..))

newtype ItemForm r f = ItemForm (r
  ( id :: f Void ItemId ItemId
  , title :: f Void String String
  , image :: f Void (Maybe Image) (Maybe Image)
  , token :: f Void (Maybe Token) (Maybe Token)
  , createdAt :: f Void Timestamp Timestamp
  , updatedAt :: f Void (Maybe Timestamp) (Maybe Timestamp)
  ))

derive instance newtypeItemForm :: Newtype (ItemForm r f) _

prx :: F.SProxies ItemForm
prx = F.mkSProxies (F.FormProxy :: F.FormProxy ItemForm)

type Input = 
  { item :: Maybe Item
  }

data Action
  = Initialize
  | Receive Input
  | HandleImageModal
  | HandleImageBrowserOutput (Browser.Output ImageType)
  | HandleTokenDropdown Dropdown.Message


type ChildSlots = (
  imageBrowser :: H.Slot (Const Void) (Browser.Output ImageType) Unit,
  tokenDropdown :: H.Slot (Const Void) Dropdown.Message Unit
)

type Query = Const Void

type AddedState = (
  imageBrowserActive :: Boolean,
  tokens :: Array Token
)

component :: forall m
           . MonadAff m
          => MonadEffect m
          => ManageMedia m
          => ManageToken m
          => F.Component ItemForm Query ChildSlots Input Item m
component = F.component input F.defaultSpec
  { render = render
  , handleEvent = handleEvent
  , handleAction = handleAction
  , initialize = Just Initialize
  , receive = Just <<< Receive
  }
  where
  input inp =
    { initialInputs: case inp.item of
      Just (Item item) ->
        Just $ F.wrapInputFields item
      Nothing -> Nothing
    , validators: ItemForm
      { id: F.noValidation
      , title: F.noValidation
      , image: F.noValidation
      , token: F.noValidation
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    , imageBrowserActive: false
    , tokens: []
    }

  handleAction :: Action
               -> F.HalogenM ItemForm AddedState Action ChildSlots Item m Unit
  handleAction = case _ of
    Initialize -> do
      tokens <- allTokens
      H.modify_ _ { tokens = tokens }


    HandleImageModal ->
      H.modify_ _ { imageBrowserActive = true 
                  }
    
    HandleImageBrowserOutput output -> case output of
      Browser.InsertedMedia images -> do
        state <- H.get
        let 
          modImages = map (\(Media x) -> Image x) images
        eval $ F.setValidate prx.image $ head modImages
        H.modify_ _ { imageBrowserActive = false }
      _ -> pure unit

    Receive inp -> case inp.item of
      Just (Item item) -> do
        eval $ F.setValidate prx.id item.id
        eval $ F.setValidate prx.title item.title
        eval $ F.setValidate prx.image item.image
        eval $ F.setValidate prx.token item.token
        eval $ F.setValidate prx.createdAt item.createdAt
        eval $ F.setValidate prx.updatedAt item.updatedAt

      Nothing -> pure unit

    HandleTokenDropdown msg -> case msg of
      Dropdown.SelectionChanged old new -> do
        state <- H.get
        case new of
          Just str -> do
            let
              hit = find (\(Token token) -> token.title == str) state.tokens
            case hit of
              Just h -> do
                eval $ F.setValidate prx.token hit
              Nothing -> pure unit
          Nothing -> pure unit

    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event ItemForm AddedState
              -> F.HalogenM ItemForm AddedState Action ChildSlots Item m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      let 
        fields = F.unwrapOutputFields form
      H.raise $ Item fields
    _ -> pure unit

  render :: F.PublicState ItemForm AddedState
         -> F.ComponentHTML ItemForm Action ChildSlots m
  render st =
    HH.div_ 
      [ withLabel "Title" (HH.input
        [ css "text-input"
        , HP.value $ F.getInput prx.title st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.title
        ])
      , maybeElem (F.getInput prx.image st.form) \(Image img) ->
        withLabel "Current image" (HH.img
          [ HP.src $ fromMaybe img.src img.thumbnail])

      , withLabel "Image" (HH.a
        [ css "button"
        , HE.onClick \_ -> Just $ F.injAction $ HandleImageModal
        ]
        [ HH.text "+ Add Image" ])
      , HH.slot
        (SProxy :: _ "imageBrowser")
        unit
        (Media.component)
        { isActive: st.imageBrowserActive }
        (Just <<< F.injAction <<< HandleImageBrowserOutput)
      , HH.div
        []
        [ HH.label
          []
          [ HH.text "Token" ]
        ]
      , HH.slot
        (SProxy :: _ "tokenDropdown")
        unit
        Dropdown.component
        { items: map (\(Token tkn) -> tkn.title) st.tokens
        , buttonLabel: 
          fromMaybe 
            "Select token: "
            (map (\(Token selTkn) -> selTkn.title) (F.getInput prx.token st.form))
        }
        (Just <<< F.injAction <<< HandleTokenDropdown)
      , HH.button
        [ HE.onClick \_ -> Just F.submit
        , css "button"
        ]
        [ HH.text "+ Add Item" ]
      ]

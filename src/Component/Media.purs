module Component.Media where

import Prelude
import Effect.Class                     (class MonadEffect)
import Effect.Aff.Class                 (class MonadAff)
import Effect.Class.Console             (logShow)
import Data.Const                       (Const(..))
import Data.Maybe                       (Maybe(..), fromMaybe)
import Data.Symbol                      (SProxy(..))
import Halogen                          as H
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.HTML.Properties          as HP
import Halogen.Media.Component.Browser  as Browser
import Halogen.Media.Data.Media         (Media(..)
                                        ,MediaArray)
import Halogen.Media.Utils              (filesToFormData)

import Api.Endpoint                     (Pagination)
import Data.Image                       (Image(..)
                                        ,ImageType)
import Resource.Media                   (class ManageMedia
                                        ,getImages
                                        ,uploadImage)

data Action
  = Initialize
  | LoadMedia Pagination
  | HandleBrowserAction (Browser.Output ImageType)
  | Receive Input

type Input =
  { loadImages :: Boolean
  }

type ChildSlots = (
  mediaBrowser :: H.Slot (Const Void) (Browser.Output ImageType) Unit
)

type State =
  { images :: MediaArray ImageType
  , imagesHaveLoaded :: Boolean
  }

component :: forall m
           . MonadAff m
          => ManageMedia m
          => H.Component HH.HTML (Const Void) Input (Browser.Output ImageType) m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  initialState :: State
  initialState =
    { images: []
    , imagesHaveLoaded: false
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots (Browser.Output ImageType) m Unit
  handleAction = case _ of
    Initialize -> do
      void $ H.fork 
           $ handleAction 
           $ LoadMedia { page: Just 1, perPage: Just 25 }

    LoadMedia pagination -> do
      imgs <- getImages pagination
      H.modify_ _ { images = map (\(Image x) -> Media x) imgs }
    HandleBrowserAction act -> do
      case act of
        Browser.TabSwitch tab -> do
          case tab of
            Browser.DisplayTab ->
              void $ H.fork $ handleAction $ LoadMedia { page: Just 1, perPage: Just 25 }
            _ -> pure unit
          H.raise act
        Browser.Upload   files -> do
          formData <- H.liftEffect $ filesToFormData "image" files
          newImg <- uploadImage formData
          pure unit
        _ -> H.raise act
    Receive inp -> case inp.loadImages of
      true -> do
        state <- H.get
        case state.imagesHaveLoaded of 
          true -> pure unit
          false -> do
            imgs <- getImages { page: Just 1, perPage: Just 25 }
            H.modify_ _ { images = map (\(Image x) -> Media x) imgs }
            H.modify_ _ { imagesHaveLoaded = true }
      false -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.slot
    (SProxy :: _ "mediaBrowser")
    unit
    Browser.component
    { media: state.images
    , selectedTab: Nothing
    }
    (Just <<< HandleBrowserAction)


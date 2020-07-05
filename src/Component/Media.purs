module Component.Media where 

import Prelude
import Data.Array                       (length)
import Data.Const                       (Const)
import Data.Maybe                       (Maybe(..))
import Data.Symbol                      (SProxy(..))
import Data.Traversable                 (traverse)
import Effect.Class.Console             (logShow)
import Effect.Class                     (class MonadEffect)
import Effect.Aff.Class                 (class MonadAff)
import Halogen                          as H
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.Media.Component.Browser  as Browser
import Halogen.Media.Component.Modal    as Modal
import Halogen.Media.Data.Media         (Media(..)
                                        ,MediaArray)
import Halogen.Media.Utils              (fileToFormData)

import Halogen.Media.Data.File          (ExtendedFile(..))
import Formless                         as F

import Component.HTML.Utils             (css)
import Data.Image                       (Image(..)
                                        ,ImageId(..)
                                        ,ImageArray
                                        ,ImageType)
import Resource.Media                   (class ManageMedia
                                        ,getImages
                                        ,deleteImage
                                        ,uploadImage)

type Query = Const Void

type ChildSlots = (
  mediaModal :: H.Slot (Modal.Query ImageType) (Browser.Output ImageType) Unit
)

data Action
  = Receive Input
  | HandleBrowserAction (Browser.Output ImageType)

type Input =
  { isActive :: Boolean
  }

type State =
  { isActive :: Boolean
  , media :: ImageArray
  }

initialState :: Input -> State
initialState inp = 
  { isActive: inp.isActive
  , media: []
  }

component :: forall m
           . MonadEffect m
          => MonadAff m
          => ManageMedia m
          => H.Component HH.HTML Query Input (Browser.Output ImageType) m
component = 
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where
  handleAction :: Action 
               -> H.HalogenM State Action ChildSlots (Browser.Output ImageType) m Unit
  handleAction = case _ of 
    HandleBrowserAction act -> case act of
      Browser.TabSwitch tab -> do
        case tab of
          Browser.DisplayTab -> do
            medias <- getImages { page: Just 1, perPage: Just 25 }
            H.modify_ _ { media = medias }
          _ -> pure unit
      Browser.Dropped files -> do
        _ <- traverse  (\(ExtendedFile f uuid t) -> do
          formData <- H.liftEffect $ fileToFormData "image" (ExtendedFile f uuid t)
          newImg <- uploadImage formData
          H.query (SProxy :: SProxy "mediaModal") unit (H.tell (Modal.SetUploadStatus uuid true))
        ) files
        H.raise act
      Browser.Removed (Media media) -> do
        deleteImage media.id

        -- TODO: We should handle this in terms of pagination
        medias <- getImages 
          { page: Just 1
          , perPage: Just 25 
          }
        H.modify_ _ { media = medias }
      _ -> H.raise act
    Receive inp -> do
      state <- H.get
      case inp.isActive of
        true -> case length state.media of
          0 -> do
            medias <- getImages { page: Just 1, perPage: Just 25 }
            H.modify_ _ { media = medias }
          _ -> pure unit
        false -> pure unit

      H.modify_ _ { isActive = inp.isActive
                  }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.slot 
    (SProxy :: _ "mediaModal")
    unit
    Modal.component
    { isActive: state.isActive
    , media: map (\(Image x) -> Media x) state.media
    }
    (Just <<< HandleBrowserAction)

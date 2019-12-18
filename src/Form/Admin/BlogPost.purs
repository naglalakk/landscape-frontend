module Form.Admin.BlogPost where

import Prelude

import Control.Monad.Except             (runExcept)
import Control.Monad.Except.Trans       (runExceptT)
import Data.Argonaut                    (encodeJson
                                        ,decodeJson)
import Data.Argonaut.Core               (Json)
import Data.Array                       (head
                                        ,filter
                                        ,length)
import Data.Const                       (Const(..))
import Data.Either                      (Either(..))
import Data.Maybe                       (Maybe(..)
                                        ,fromMaybe)
import Data.Newtype                     (class Newtype)
import Data.Number.Format               as Number
import Data.String.Utils                (startsWith)
import Data.Symbol                      (SProxy(..))
import Data.Traversable                 (traverse)
import Effect.Aff                       as Aff
import Effect.Aff.Class                 (class MonadAff)
import Effect.Class                     (class MonadEffect)
import Effect.Class.Console             (logShow)
import Foreign                          (unsafeFromForeign)
import Foreign                          as Foreign
import Foreign.Generic                  (genericDecodeJSON
                                        ,decodeJSON
                                        ,encodeJSON)
import Foreign.Generic.Class            (encode, decode)
import Foreign.Object                   as Object
import Formless                         as F
import Halogen                          as H
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.HTML.Properties          as HP
import Halogen.Media.Component.Browser  as Browser
import Halogen.Media.Component.Upload   as Upload
import Halogen.Media.Data.Media         (Media(..))
import Halogen.Media.Utils              (filesToFormData)
import Halogen.Query.EventSource        as HES
import Quill.API.Delta                  as QDelta
import Timestamp (Timestamp, nowTimestamp, formatToDateTimeShortStr, defaultTimestamp)


import Component.Editor as Editor
import Component.HTML.Utils (css, withLabel)
import Component.Modal as Modal
import Data.BlogPost (BlogPost(..), BlogPostId)
import Data.Image (Image(..), ImageType)
import Form.Error (FormError(..))
import Form.Validation (validateStr, validateDateTime)
import Resource.Media (class ManageMedia)
import Utils.Record (fromTimestampField)


newtype BlogPostForm r f = BlogPostForm (r
  ( id              :: f Void       BlogPostId          BlogPostId
  , title           :: f FormError  String              String
  , content         :: f Void       String              String
  , htmlContent     :: f Void       (Maybe String)      (Maybe String)
  , featuredImage   :: f Void       (Maybe Image)       (Maybe Image)
  , publishTime     :: f FormError  String              Timestamp
  , createdAt       :: f Void       Timestamp           Timestamp
  , updatedAt       :: f Void       (Maybe Timestamp)   (Maybe Timestamp)
  ))

derive instance newtypeBlogPostForm :: Newtype (BlogPostForm r f) _

prx :: F.SProxies BlogPostForm
prx = F.mkSProxies (F.FormProxy :: F.FormProxy BlogPostForm)

type Input = 
  { blogPost :: Maybe BlogPost
  }

type AddedState = (
  featuredModalActive :: Boolean
)

data Action
  = Initialize
  | Receive Input
  | HandleFeaturedBrowserOutput (Browser.Output ImageType)
  | HandleFeaturedBrowserModal
  | RemoveFeaturedImage Image
  | HandleEditorDelta
  | SubmitForm

type ChildSlots = (
  featuredModal :: H.Slot (Const Void) (Browser.Output ImageType) Unit,
  editor :: H.Slot Editor.Query Void Unit
)

component :: forall m
           . Monad m
          => MonadAff m
          => MonadEffect m
          => ManageMedia m
          => F.Component BlogPostForm (Const Void) ChildSlots Input BlogPost m
component = F.component input F.defaultSpec
  { render = render
  , handleEvent = handleEvent
  , handleAction = handleAction
  , initialize = Just Initialize
  , receive = Just <<< Receive
  }
  where

  handleAction :: Action
               -> F.HalogenM BlogPostForm AddedState Action ChildSlots BlogPost m Unit
  handleAction = case _ of
    Initialize -> pure unit

    HandleEditorDelta -> do

      -- Fetch latest delta as Text
      query <- H.query (SProxy :: SProxy "editor") unit (H.request Editor.GetText)
      case query of
        Just content -> do
          eval $ F.setValidate prx.content content
        Nothing  -> pure unit

      -- We save the raw html separately
      htmlQuery <- H.query (SProxy :: SProxy "editor") unit (H.request Editor.GetHTMLText)
      case htmlQuery of 
        Just hContent -> 
          eval $ F.setValidate prx.htmlContent 
               $ Just hContent
        Nothing -> pure unit

      -- handleAction HandleEditorDelta

    Receive inp -> do
      case inp.blogPost of
        Just (BlogPost blogPost) -> do
          eval $ F.setValidate prx.id blogPost.id
          eval $ F.setValidate prx.title blogPost.title
          eval $ F.setValidate prx.content blogPost.content
          eval $ F.setValidate prx.htmlContent blogPost.htmlContent
          eval $ F.setValidate prx.publishTime 
               $ formatToDateTimeShortStr blogPost.publishTime
          eval $ F.setValidate prx.createdAt blogPost.createdAt
          eval $ F.setValidate prx.updatedAt blogPost.updatedAt

        Nothing -> pure unit
      H.modify_ _ { featuredModalActive = false }

    HandleFeaturedBrowserOutput output -> case output of
      Browser.InsertedMedia images -> do
        state <- H.get
        let 
          modImages = map (\(Media x) -> Image x) images
          mainImage = head modImages
        
        eval $ F.setValidate prx.featuredImage mainImage
        H.modify_ _ { featuredModalActive = false }
      _ -> pure unit

    HandleFeaturedBrowserModal -> 
      H.modify_ _ { featuredModalActive = true }
    
    RemoveFeaturedImage (Image image) ->
      eval $ F.setValidate prx.featuredImage Nothing
    
    SubmitForm -> do
      handleAction HandleEditorDelta
      eval F.submit

    where
      eval act = F.handleAction handleAction handleEvent act



  handleEvent :: F.Event BlogPostForm AddedState
              -> F.HalogenM BlogPostForm AddedState Action ChildSlots BlogPost m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      let 
        fields = F.unwrapOutputFields form
      H.raise $ BlogPost fields
    _ -> pure unit

  input :: Input -> F.Input BlogPostForm AddedState m
  input inp =
    { initialInputs: case inp.blogPost of
      Just (BlogPost bp) -> 
        Just 
        $ F.wrapInputFields 
        $ fromTimestampField (SProxy :: SProxy "publishTime")
        bp
      Nothing -> Nothing
    , validators: BlogPostForm
      { id: F.noValidation
      , title: validateStr
      , content: F.noValidation
      , htmlContent: F.noValidation
      , featuredImage: F.noValidation
      , publishTime: validateDateTime
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    , featuredModalActive: false
    }

  render :: F.PublicState BlogPostForm AddedState
         -> F.ComponentHTML BlogPostForm Action ChildSlots m
  render st =
    HH.form_
      [ withLabel "Title" (HH.input
        [ css "text-input"
        , HP.value $ F.getInput prx.title st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.title
        ])
      , withLabel "Publish time" (HH.input
        [ css "text-input" 
        , HP.value $ F.getInput prx.publishTime st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.publishTime
        ])
      , HH.div
        [ css "editor" ]
        [ HH.label
          [ css "label" ]
          [ HH.text "Content" ]
        , HH.slot
          (SProxy :: _ "editor")
          unit
          Editor.component
          { content: Just content }
          (\_ -> Nothing)
        ]
      , HH.button
        [ css "button"
        , HE.onClick \_ -> Just $ F.injAction SubmitForm
        ]
        [ HH.text "Save" ]
      ]
    where
      content = F.getInput prx.content st.form

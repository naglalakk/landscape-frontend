module Form.Admin.BlogPost where

import Prelude

import Component.Editor as Editor
import Component.HTML.Utils (css, withLabel)
import Component.Modal as Modal
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (runExceptT)
import Data.Argonaut (encodeJson, decodeJson)
import Data.Argonaut.Core (Json)
import Data.Array (head, filter, length)
import Data.BlogPost (BlogPost(..), BlogPostId)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Image (Image(..), ImageType)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Number.Format as Number
import Data.String.Utils (startsWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Foreign (unsafeFromForeign)
import Foreign as Foreign
import Foreign.Generic (genericDecodeJSON, decodeJSON, encodeJSON)
import Foreign.Generic.Class (encode, decode)
import Foreign.Object as Object
import Form.Error (FormError(..))
import Form.Validation (validateStr, validateDateTime)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Media.Component.Browser as Browser
import Halogen.Media.Component.Upload as Upload
import Halogen.Media.Data.Media (Media(..))
import Halogen.Media.Utils (filesToFormData)
import Halogen.Query.EventSource as HES
import Quill.API.Delta as QDelta
import Resource.Media (class ManageMedia)
import Timestamp (Timestamp, nowTimestamp, formatToDateTimeShortStr, defaultTimestamp)
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
  featuredModalActive :: Boolean,
  editorOps :: Maybe QDelta.Ops
)

data Action
  = Initialize
  | Receive Input
  | HandleFeaturedBrowserOutput (Browser.Output ImageType)
  | HandleFeaturedBrowserModal
  | RemoveFeaturedImage Image
  | HandleEditorDelta

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
    Initialize -> do
      void $ H.fork $ handleAction $ HandleEditorDelta 

    HandleEditorDelta -> do
      H.liftAff $ Aff.delay $ Aff.Milliseconds 3000.0
      query <- H.query (SProxy :: SProxy "editor") unit (H.request Editor.GetText)
      case query of
        Just (QDelta.Ops deltas) -> do
          -- encode ops
          let 
            encodedOps = encodeJSON $ QDelta.Ops deltas
          logShow encodedOps
          eval $ F.setValidate prx.content encodedOps
        Nothing  -> pure unit
      handleAction HandleEditorDelta

    Receive inp -> do
      case inp.blogPost of
        Just (BlogPost blogPost) -> do
          let 
            contentDeltas :: Either Foreign.MultipleErrors QDelta.Ops
            contentDeltas = runExcept $ decodeJSON blogPost.content
          case contentDeltas of
            Right ops -> do
              H.modify_ _ { editorOps = Just ops }
            Left err -> pure unit
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

    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event BlogPostForm AddedState
              -> F.HalogenM BlogPostForm AddedState Action ChildSlots BlogPost m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      query <- H.query (SProxy :: SProxy "editor") unit (H.request Editor.GetHTMLText)
      let 
        fields = (F.unwrapOutputFields form) { htmlContent = query }
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
    , editorOps: Nothing
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
          { content: st.editorOps }
          (\_ -> Nothing)
        ]
      , HH.button
        [ css "button"
        , HE.onClick \_ -> Just F.submit
        ]
        [ HH.text "Save" ]
      ]

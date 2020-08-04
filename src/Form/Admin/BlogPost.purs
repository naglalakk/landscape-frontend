module Form.Admin.BlogPost where

import Prelude

import Control.Monad.Except             (runExcept)
import Control.Monad.Except.Trans       (runExceptT)
import Data.Argonaut                    (encodeJson
                                        ,decodeJson)
import Data.Argonaut.Core               (Json)
import Data.Array                       (head
                                        ,filter
                                        ,length
                                        ,catMaybes)
import Data.Const                       (Const(..))
import Data.Either                      (Either(..))
import Data.Maybe                       (Maybe(..)
                                        ,fromMaybe)
import Data.Newtype                     (class Newtype, unwrap)
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
import Halogen.Query.EventSource        as HES
import Quill.API.Delta                  as QDelta
import Slug                             as Slug
import Timestamp                        (Timestamp
                                        ,nowTimestamp
                                        ,formatToDateTimeShortStr
                                        ,defaultTimestamp)


import Component.Editor                 as Editor
import Component.HTML.Utils             (css
                                        ,maybeElem
                                        ,safeHref
                                        ,withLabel)
import Component.Media                  as Media
import Data.BlogPost                    (BlogPost(..)
                                        ,BlogPostId)
import Data.Image                       (Image(..)
                                        ,ImageType
                                        ,ImageArray)
import Data.Tag                         (Tag(..), TagArray)
import Data.Route                       as R
import Foreign.Flatpickr                (loadFlatpickr)
import Foreign.Tagify                   as Tagify
import Form.Error                       (FormError(..))
import Form.Validation                  (validateStr
                                        ,validateDateTime)
import Resource.Media                   (class ManageMedia)
import Resource.Tag                     (class ManageTag, createTag)
import Utils.Record                     (fromTimestampField)


newtype BlogPostForm r f = BlogPostForm (r
  ( id              :: f Void       BlogPostId          BlogPostId
  , title           :: f FormError  String              String
  , slug            :: f Void       (Maybe Slug.Slug)   (Maybe Slug.Slug) 
  , content         :: f Void       String              String
  , htmlContent     :: f Void       (Maybe String)      (Maybe String)
  , featuredImage   :: f Void       (Maybe Image)       (Maybe Image)
  , images          :: f Void       ImageArray          ImageArray
  , tags            :: f Void       TagArray            TagArray
  , published       :: f Void       Boolean             Boolean
  , publishTime     :: f FormError  String              Timestamp
  , showDate        :: f Void       Boolean             Boolean
  , isCover         :: f Void       Boolean             Boolean
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
  featuredImageBrowserActive :: Boolean,
  imageBrowserActive :: Boolean,
  tagify :: Maybe Tagify.Tagify
)

data Action
  = Initialize
  | AutoSave
  | Receive Input
  | HandleFeaturedImageBrowserOutput (Browser.Output ImageType)
  | HandleFeaturedImageModal
  | RemoveFeaturedImage
  | HandleImageModal
  | HandleImageBrowserOutput (Browser.Output ImageType)
  | RemoveImage Image
  | HandleEditorDelta
  | SubmitForm

type ChildSlots = (
  imageBrowser :: H.Slot (Const Void) (Browser.Output ImageType) Unit,
  featuredImageBrowser:: H.Slot (Const Void) (Browser.Output ImageType) Unit,
  editor :: H.Slot Editor.Query Void Unit
)

component :: forall m
           . Monad m
          => MonadAff m
          => MonadEffect m
          => ManageMedia m
          => ManageTag m
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
      H.liftEffect $ loadFlatpickr ".datetime"
      -- void $ H.fork $ handleAction AutoSave
      -- Init tagify instance
      tagify <- H.liftEffect $ Tagify.createTagify "tags"
      H.modify_ _ { tagify = Just tagify }

    AutoSave -> do
      -- Autosave every 3 seconds
      H.liftAff $ Aff.delay $ Aff.Milliseconds 3000.0
      handleAction SubmitForm
      handleAction AutoSave

    HandleEditorDelta -> do
      -- Fetch latest delta as Text
      query <- H.query (SProxy :: SProxy "editor") unit (H.request Editor.GetText)
      logShow query
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

    Receive inp -> do
      case inp.blogPost of
        Just (BlogPost blogPost) -> do
          eval $ F.setValidate prx.id blogPost.id
          eval $ F.setValidate prx.title blogPost.title
          eval $ F.setValidate prx.slug blogPost.slug
          eval $ F.setValidate prx.content blogPost.content
          eval $ F.setValidate prx.htmlContent blogPost.htmlContent
          eval $ F.setValidate prx.featuredImage blogPost.featuredImage
          eval $ F.setValidate prx.images blogPost.images
          eval $ F.setValidate prx.tags blogPost.tags
          eval $ F.setValidate prx.published blogPost.published
          eval $ F.setValidate prx.publishTime 
               $ formatToDateTimeShortStr blogPost.publishTime
          eval $ F.setValidate prx.showDate blogPost.showDate
          eval $ F.setValidate prx.isCover blogPost.isCover
          eval $ F.setValidate prx.createdAt blogPost.createdAt
          eval $ F.setValidate prx.updatedAt blogPost.updatedAt

          -- handle tags
          state <- H.get
          case state.tagify of
            Just tagify -> do
              let strTags = map (\(Tag tag) -> tag.label) blogPost.tags
              H.liftEffect $ Tagify.addTags tagify strTags
            Nothing -> pure unit
        Nothing -> pure unit
      H.modify_ _ { featuredImageBrowserActive = false 
                  , imageBrowserActive = false
                  }

    HandleFeaturedImageBrowserOutput output -> case output of
      Browser.InsertedMedia images -> do
        state <- H.get
        let 
          modImages = map (\(Media x) -> Image x) images
          mainImage = head modImages
        
        eval $ F.setValidate prx.featuredImage mainImage
        H.modify_ _ { featuredImageBrowserActive = false }
      _ -> pure unit
    
    HandleFeaturedImageModal -> 
      H.modify_ _ { featuredImageBrowserActive = true 
                  , imageBrowserActive = false
                  }
    
    RemoveFeaturedImage ->
      eval $ F.setValidate prx.featuredImage Nothing

    HandleImageModal ->
      H.modify_ _ { imageBrowserActive = true 
                  , featuredImageBrowserActive = false
                  }
    
    HandleImageBrowserOutput output -> case output of
      Browser.InsertedMedia images -> do
        state <- H.get
        let 
          modImages = map (\(Media x) -> Image x) images
        eval $ F.setValidate prx.images modImages
        H.modify_ _ { imageBrowserActive = false }
      _ -> pure unit

    RemoveImage (Image image) -> pure unit

    SubmitForm -> do
      state <- H.get
      handleAction HandleEditorDelta
      -- get tags
      case state.tagify of
        Just tF -> do
          strTags <- H.liftEffect $ Tagify.getTags tF
          tags <- traverse createTag strTags
          let finalTags = catMaybes tags
          eval $ F.setValidate prx.tags finalTags
        Nothing -> pure unit
      eval F.submit

    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event BlogPostForm AddedState
              -> F.HalogenM BlogPostForm AddedState Action ChildSlots BlogPost m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      let 
        fields = F.unwrapOutputFields form
      let 
        slug = Slug.generate fields.title
        finalFields = case fields.slug == slug of
          true -> fields
          false -> fields { slug = slug }
      H.modify_ _ { imageBrowserActive = false
                  , featuredImageBrowserActive = false 
                  }
      H.raise $ BlogPost finalFields
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
      , slug: F.noValidation
      , content: F.noValidation
      , htmlContent: F.noValidation
      , featuredImage: F.noValidation
      , images: F.noValidation
      , tags: F.noValidation
      , published: F.noValidation
      , publishTime: validateDateTime
      , showDate: F.noValidation
      , isCover: F.noValidation
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    , featuredImageBrowserActive: false
    , imageBrowserActive: false
    , tagify: Nothing
    }

  render :: F.PublicState BlogPostForm AddedState
         -> F.ComponentHTML BlogPostForm Action ChildSlots m
  render st =
    HH.div_
      [ withLabel "Title" (HH.input
        [ css "text-input"
        , HP.value $ F.getInput prx.title st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.title
        ])
      , withLabel "Publish time" (HH.input
        [ css "text-input datetime" 
        , HP.value $ F.getInput prx.publishTime st.form
        , HE.onValueInput $ Just <<< F.setValidate prx.publishTime
        ])
      , withLabel "Show publish time" (HH.div
        []
        [ HH.div
          [ css "description" ]
          [ HH.text "Uncheck this if you don't want to show the date below the header title." ]
        , HH.input
          [ HP.checked $ F.getInput prx.showDate st.form
          , HP.type_ HP.InputCheckbox
          , HE.onChecked \_ -> Just $ F.modify prx.showDate not
          ]
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
      , HH.div
        [ css "field-featuredImage" ]
        [ HH.label
          [ css "label" ]
          [ HH.text "Featured Image" ]
        , HH.a
          [ css "button" 
          , HE.onClick \_ -> Just $ F.injAction $ HandleFeaturedImageModal  
          ]
          [ HH.text "+ Choose Image" ]
        , HH.div
          [ css "image-preview" ]
          [ maybeElem featuredImage \(Image image) ->
            HH.div
              [ css "image-container"]
              [ HH.div
                [ css "image-preview" ]
                [ HH.div
                  [ css "image-preview-overlay" ]
                  [ HH.div
                    [ css "image-preview-remove-icon" ]
                    [ HH.i
                      [ css "fas fa-trash" 
                      , HE.onClick \_ -> Just $ F.injAction $ RemoveFeaturedImage
                      ]
                      []
                    ]
                  ]
                , HH.img
                  [ HP.src image.src 
                  ]
                ]
              ]
          ]
        , HH.slot
          (SProxy :: _ "featuredImageBrowser")
          unit
          (Media.component)
          { isActive: st.featuredImageBrowserActive }
          (Just <<< F.injAction <<< HandleFeaturedImageBrowserOutput)
        ]
      , HH.div
        [ css "field-images" ]
        [ HH.label
          [ css "label" ]
          [ HH.text "Images in post" ]
        , HH.a
          [ css "button"
          , HE.onClick \_ -> Just $ F.injAction $ HandleImageModal  
          ]
          [ HH.text "+ Add Image" ]
        , HH.div
          [ css "image-container"]
          (map (\(Image image) -> 
            HH.div
              [ css "image-preview" ]
              [ HH.div
                [ css "image-preview-overlay" ]
                [ HH.div
                  [ css "image-preview-remove-icon" ]
                  [ HH.i
                    [ css "fas fa-trash" 
                    , HE.onClick \_ -> Just $ F.injAction $ RemoveImage (Image image)
                    ]
                    []
                  ]
                ]
              , HH.img
                [ HP.src image.src 
                ]
              ]) images)
        , HH.slot
          (SProxy :: _ "imageBrowser")
          unit
          (Media.component)
          { isActive: st.imageBrowserActive }
          (Just <<< F.injAction <<< HandleImageBrowserOutput)
        ]
      , withLabel "Include featuredImage as cover photo" (HH.div
        []
        [ HH.div
          [ css "description" ]
          [ HH.text "Keep this checked if you want to have the featured image as a cover photo for this post. Cover photos appear in full width and have a parallax effect on scroll" ]
        , HH.input
          [ HP.checked $ F.getInput prx.isCover st.form
          , HP.type_ HP.InputCheckbox
          , HE.onChecked \_ -> Just $ F.modify prx.isCover not
          ]
        ])
      , withLabel "Tags" (HH.div
        [ css "admin-post-tags" ]
        [ HH.input
          [ HP.id_ "tags"
          ]
        ])
      , maybeElem slug \s ->
        HH.div
        [ css "post-url" ]
        [ HH.span
          []
          [ HH.text "Post URL: " ]
        , HH.a
          [ safeHref $ R.BlogPost $ Slug.toString s ]
          [ HH.text $ "/#/" <> Slug.toString s ]
        ]
      , HH.button
        [ css "button"
        , HE.onClick \_ -> Just $ F.injAction SubmitForm
        ]
        [ HH.text "Save" ]
      ]
    where
      slug    = F.getInput prx.slug st.form
      content = F.getInput prx.content st.form
      images  = F.getInput prx.images st.form
      featuredImage = F.getInput prx.featuredImage st.form

module Form.Admin.BlogPost where

import Prelude

import Effect.Class                     (class MonadEffect)
import Effect.Aff.Class                 (class MonadAff)
import Effect.Class.Console             (logShow)
import Data.Array                       (head, filter, length)
import Data.Const                       (Const(..))
import Data.Maybe                       (Maybe(..), fromMaybe)
import Data.Newtype                     (class Newtype)
import Data.Number.Format               as Number
import Data.Symbol                      (SProxy(..))
import Data.String.Utils                (startsWith)
import Formless                         as F
import Halogen                          as H
import Halogen.Media.Component.Upload   as Upload
import Halogen.Media.Component.Browser  as Browser
import Halogen.Media.Data.Media         (Media(..))
import Halogen.Media.Utils              (filesToFormData)
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.HTML.Properties          as HP
import Timestamp                        (Timestamp
                                        ,nowTimestamp
                                        ,formatToDateTimeShortStr
                                        ,defaultTimestamp)

import Component.Modal                  as Modal
import Data.BlogPost                    (BlogPost(..), BlogPostId)
import Data.Image                       (Image(..), ImageType)
import Form.Error                       (FormError(..))
import Form.Validation                  (validateStr
                                        ,validateDateTime)
import Resource.Media                   (class ManageMedia)
import Utils.Record                     (fromTimestampField)


newtype BlogPostForm r f = BlogPostForm (r
  ( id              :: f Void       BlogPostId          BlogPostId
  , title           :: f FormError  String              String
  , content         :: f Void       String              String
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
  = Receive Input
  | HandleFeaturedBrowserOutput (Browser.Output ImageType)
  | HandleFeaturedBrowserModal
  | RemoveFeaturedImage Image

type ChildSlots = (
  featuredModal :: H.Slot (Const Void) (Browser.Output ImageType) Unit
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
  , receive = Just <<< Receive
  }
  where
  handleAction :: Action
               -> F.HalogenM BlogPostForm AddedState Action ChildSlots BlogPost m Unit
  handleAction = case _ of
    Receive inp -> do
      case inp.blogPost of
        Just (BlogPost blogPost ) -> do
          eval $ F.setValidate prx.id blogPost.id
          eval $ F.setValidate prx.title blogPost.title
          eval $ F.setValidate prx.content blogPost.content
          eval $ F.setValidate prx.publishTime $ formatToDateTimeShortStr blogPost.publishTime
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
    HH.div [] []

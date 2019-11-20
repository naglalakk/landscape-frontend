module Page.Admin.BlogPost where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Newtype                 (unwrap)
import Data.Symbol                  (SProxy(..))
import Effect.Aff.Class             (class MonadAff)
import Effect.Class                 (class MonadEffect)
import Effect.Class.Console         (logShow)
import Formless                     as F
import Halogen                      as H
import Halogen.HTML                 as HH

import Api.Endpoint                 (Pagination)
import Capability.Navigate          (class Navigate
                                    ,navigate)
import Component.Utils              (OpaqueSlot)
import Component.HTML.Admin         (withAdmin)
import Component.HTML.Utils         (css)
import Component.Table              as Table
import Data.BlogPost                (BlogPost(..)
                                    ,BlogPostId(..)
                                    ,BlogPostArray)
import Data.Route                   as R
import Form.Admin.BlogPost          as BlogPostForm
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPost
                                    ,createBlogPost
                                    ,updateBlogPost)
import Resource.Media               (class ManageMedia)
import Timestamp                    (formatToDateStr)

type Input = 
  { blogPostId :: BlogPostId
  }

type State = 
  { blogPostId :: BlogPostId
  , blogPost :: Maybe BlogPost
  }

data Action 
  = Initialize
  | LoadBlogPost BlogPostId
  | HandleBlogPostForm BlogPost

type ChildSlots = (
  formless :: H.Slot (F.Query BlogPostForm.BlogPostForm (Const Void) BlogPostForm.ChildSlots) BlogPost Unit
)

type Query = Const Void

initialState :: Input -> State
initialState inp  = 
  { blogPostId: inp.blogPostId
  , blogPost: Nothing 
  }

component :: forall m
           . ManageBlogPost m
          => MonadAff m
          => MonadEffect m
          => ManageMedia m
          => Navigate m
          => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      void $ H.fork 
           $ handleAction 
           $ LoadBlogPost state.blogPostId

    HandleBlogPostForm blogPost -> do
      logShow blogPost

    LoadBlogPost postId -> do
      post <- getBlogPost postId
      H.modify_ _ { blogPost = post }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ withAdmin $
        HH.div
          []
          [ HH.h1
            []
            [ HH.text "Post" ]
          , blogPostFormSlot state.blogPost
          ]
      ]

    where
      blogPostFormSlot blogPost = 
        HH.slot
        F._formless
        unit
        (BlogPostForm.component)
        { blogPost: blogPost }
        (Just <<< HandleBlogPostForm)

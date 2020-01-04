module Page.Admin.BlogPosts where

import Prelude
import Data.Array                   (filter)
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Newtype                 (unwrap)
import Data.Symbol                  (SProxy(..))
import Effect.Class                 (class MonadEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Events          as HE

import Api.Endpoint                 (Pagination)
import Capability.Navigate          (class Navigate
                                    ,navigateForm)
import Component.Utils              (OpaqueSlot)
import Component.HTML.Admin         (withAdmin)
import Component.HTML.Utils         (css)
import Component.Table              as Table
import Data.BlogPost                (BlogPost(..)
                                    ,BlogPostId(..)
                                    ,BlogPostArray)
import Data.Route                   as R
import Resource.BlogPost            (class ManageBlogPost
                                    ,getBlogPosts
                                    ,createBlogPost
                                    ,updateBlogPost
                                    ,deleteBlogPost)
import Timestamp                    (formatToDateStr)

type State = 
  { blogPosts :: BlogPostArray
  }

data Action 
  = Initialize
  | LoadBlogPosts Pagination
  | HandleTableAction Table.Output
  | NavigateToBlogPost BlogPostId

type ChildSlots = (
  table :: H.Slot (Const Void) Table.Output Unit
)

initialState :: State
initialState = 
  { blogPosts: []
  }

component :: forall m
           . ManageBlogPost m
          => MonadEffect m
          => Navigate m
          => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> do
      void $ H.fork 
           $ handleAction 
           $ LoadBlogPosts { page: Just 1, perPage: Just 25 }

    LoadBlogPosts pagination -> do
      posts <- getBlogPosts pagination
      H.modify_ _ { blogPosts = posts }

    HandleTableAction act -> do
      case act of
        Table.UpdateRow row ->
          navigateForm $ R.AdminBlogPost $ BlogPostId row.id
        Table.DeleteRow row -> do
          _ <- deleteBlogPost $ BlogPostId row.id
          state <- H.get
          let posts = filter (\(BlogPost x) -> 
            (unwrap x.id) /= row.id) state.blogPosts
          H.modify_ _ { blogPosts = posts }

    NavigateToBlogPost blogPostId ->
      navigateForm $ R.AdminBlogPost blogPostId

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ withAdmin $
        HH.div
          []
          [ HH.div
            [ css "top-bar" ]
            [ HH.h1
              []
              [ HH.text "Posts" ]
            , HH.button
              [ css "button" 
              , HE.onClick \_ -> Just <<< NavigateToBlogPost $ BlogPostId 0
              ]
              [ HH.text "+ New Post" 
              ]
            ]
          , (tableSlot headers (rows state.blogPosts))
          ]
      ]
    where
      tableSlot h r = 
        HH.slot 
        (SProxy :: _ "table") 
        unit 
        (Table.component) 
        { rows: r, headers: h }
        (Just <<< HandleTableAction)

      headers = [ "Title", "Update", "Delete" ]
      rows posts = map (\(BlogPost x) -> 
        { id: (unwrap x.id)
        , label: x.title 
        }) posts

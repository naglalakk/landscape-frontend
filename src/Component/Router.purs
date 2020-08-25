module Component.Router where

import Prelude
import Control.Monad.Error.Class        (class MonadError)
import Control.Monad.Reader             (class MonadAsk, asks)
import Data.Either                      (hush)
import Data.Foldable                    (elem)
import Data.Maybe                       (Maybe(..)
                                        ,fromMaybe
                                        ,isJust)
import Data.String                      (drop)
import Data.Symbol                      (SProxy(..))
import Effect.Aff.Class                 (class MonadAff)
import Effect.Class.Console             (logShow)
import Effect.Ref                       as Ref
import Foreign                          as Foreign
import Halogen                          (liftEffect)
import Halogen                          as H
import Halogen.HTML                     as HH
import Routing.Duplex                   as RD
import Routing.Hash                     (getHash)
import Slug                             as Slug
import Web.HTML                         (window)
import Web.HTML.Window                  as Window
import Web.HTML.Location                as Location


import Capability.Navigate              (class Navigate, navigate)
import Component.Utils                  (OpaqueSlot, busEventSource)
import Component.HTML.Header            (header)
import Component.HTML.Utils             (css)
import Data.Environment                 (UserEnv(..))
import Data.Route                       (Route(..), routeCodec)
import Data.User                        (User(..))
import Page.Home                        as Home
import Page.BlogPost                    as BlogPost
import Page.Tag                         as TagPage
import Page.Admin.Home                  as AdminHome
import Page.Admin.BlogPosts             as AdminBlogPosts
import Page.Admin.BlogPost              as AdminBlogPost
import Page.Login                       as Login
import Resource.BlogPost                (class ManageBlogPost)
import Resource.Media                   (class ManageMedia)
import Resource.User                    (class ManageUser)
import Resource.Tag                     (class ManageTag)

type State = 
  { route :: Maybe Route 
  , currentUser :: Maybe User
  , darkMode :: Boolean
  }

data Query a
  = Navigate Route a 

data Action 
  = Initialize
  | HandleUserBus (Maybe User)
  | DarkModeToggle

type ChildSlots = 
  ( home :: OpaqueSlot Unit 
  , login :: OpaqueSlot Unit
  , blogPost :: OpaqueSlot Unit
  , tag :: OpaqueSlot Unit
  , adminHome :: OpaqueSlot Unit
  , adminBlogPosts :: OpaqueSlot Unit
  , adminBlogPost :: OpaqueSlot Unit
  )

-- Container including header
headerContainer :: forall props act
                 . Boolean           -- Dark mode off/on
                ->  act              -- Dark mode action
                -> HH.HTML props act -- html
                -> HH.HTML props act
headerContainer dmStatus dmAction slot =
  HH.div
    [ css $ "wrapper dark-mode-" <> (show dmStatus) ]
    [ header dmAction 
    , slot
    ]

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageBlogPost m
  => ManageMedia m
  => ManageUser m
  => ManageTag m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m 
component = H.mkComponent 
  { initialState: \_ -> 
    { route: Nothing
    , currentUser: Nothing
    , darkMode: false
    }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      w <- H.liftEffect window
      location <- liftEffect $ Window.location w
      p <- H.liftEffect $ Location.pathname location
      let 
        finalPath = drop 1 p
        initialRoute = hush $ (RD.parse routeCodec finalPath)
      { currentUser, userBus } <- asks _.userEnv
      _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
      mbUser <- H.liftEffect $ Ref.read currentUser
      H.modify_ _ { currentUser = mbUser 
                  , route = Just $ fromMaybe Home initialRoute
                  }

    DarkModeToggle -> do
      state <- H.get
      H.modify_ _ { darkMode = not state.darkMode }

    HandleUserBus user -> H.modify_ _ { currentUser = user }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get 
      when (route /= Just dest) do
        case (isJust currentUser && dest `elem` [ Login ]) of
          false -> H.modify_ _ { route = Just dest }
          _ -> pure unit
      pure (Just a)

  authorize :: Maybe User -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbUser html = case mbUser of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ ->
      html
  
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser, darkMode } = case route of
    Just Home -> 
      headerContainer darkMode DarkModeToggle $ HH.slot (SProxy :: _ "home") unit Home.component unit absurd
    Just (BlogPost slug) ->
      HH.slot (SProxy :: _ "blogPost") unit BlogPost.component { slug: (Slug.generate slug) } absurd
    Just Login ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: true } absurd
    Just (Tag tagId) -> 
      headerContainer darkMode DarkModeToggle $ HH.slot (SProxy :: _ "tag") unit TagPage.component { tagId: tagId } absurd
    -- Admin
    Just AdminHome ->
      HH.slot (SProxy :: _ "adminHome") unit AdminHome.component unit absurd
        # authorize currentUser
    Just AdminBlogPosts ->
      HH.slot (SProxy :: _ "adminBlogPosts") unit AdminBlogPosts.component unit absurd
        # authorize currentUser
    Just (AdminBlogPost blogPostId) -> 
      HH.slot (SProxy :: _ "adminBlogPost") unit AdminBlogPost.component { blogPostId } absurd
        # authorize currentUser
    Nothing ->
      HH.div_ [ HH.text "Oh no! That page wasn't found." ]

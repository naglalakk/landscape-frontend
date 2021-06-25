module Component.Router where

import Prelude

import CSS.Admin as AdminStyle
import CSS.Style as Style
import Capability.Navigate (class Navigate, navigate)
import Component.HTML.Header (header)
import Component.HTML.Utils (css, safeHref)
import Component.Utils (OpaqueSlot, busEventSource)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (hush)
import Data.Environment (UserEnv(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Route (Route(..), routeCodec)
import Data.String (drop)
import Data.Symbol (SProxy(..))
import Data.User (User(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (logShow)
import Effect.Ref as Ref
import Foreign as Foreign
import Foreign.Window as CWindow
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Page.Admin.BlogPost as AdminBlogPost
import Page.Admin.BlogPosts as AdminBlogPosts
import Page.Admin.Exhibition as AdminExhibition
import Page.Admin.Exhibitions as AdminExhibitions
import Page.Admin.Home as AdminHome
import Page.Admin.Token as AdminToken
import Page.Admin.Tokens as AdminTokens
import Page.Admin.Transactions as AdminTransactions
import Page.BlogPost as BlogPost
import Page.Exhibition as Exhibition
import Page.Home as Home
import Page.Login as Login
import Page.Tag as TagPage
import Resource.BlogPost (class ManageBlogPost)
import Resource.Exhibition (class ManageExhibition)
import Resource.Item (class ManageItem)
import Resource.Media (class ManageMedia)
import Resource.Tag (class ManageTag)
import Resource.Token (class ManageToken)
import Resource.User (class ManageUser)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Slug as Slug
import Web.HTML (window)
import Web.HTML.Location as Location
import Web.HTML.Window as Window

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
  | NavigateAction Route

type ChildSlots = 
  ( home :: OpaqueSlot Unit 
  , login :: OpaqueSlot Unit
  , blogPost :: OpaqueSlot Unit
  , exhibition :: OpaqueSlot Unit
  , tag :: OpaqueSlot Unit
  , adminHome :: OpaqueSlot Unit
  , adminBlogPosts :: OpaqueSlot Unit
  , adminBlogPost :: OpaqueSlot Unit
  , adminExhibitions :: OpaqueSlot Unit
  , adminExhibition :: OpaqueSlot Unit
  , adminTokens :: OpaqueSlot Unit
  , adminToken :: OpaqueSlot Unit
  , adminTransactions :: OpaqueSlot Unit
  )



-- TODO: Move to own file
adminContainer :: forall props act
                . (Route -> act)
               -> HH.HTML props act
               -> HH.HTML props act
adminContainer navigateAction slot =
  HH.div
    [ css "admin-container full-width flex-center" ]
    [ AdminStyle.stylesheet
    , HH.div
      [ css "container flex" ]
      [ HH.div
        [ css "admin-menu" ]
        [ HH.a
          [ HE.onClick \_ -> Just $ navigateAction AdminHome 
          , css "menu-link"
          ]
          [ HH.i
            [ css "fas fa-home" ]
            []
          , HH.text "Home" 
          ]
        , HH.a
          [ HE.onClick \_ -> Just $ navigateAction AdminBlogPosts
          , css "menu-link"
          ]
          [ HH.i
            [ css "fas fa-blog" ]
            []
          , HH.text "Blog" 
          ]
        , HH.a
          [ css "menu-link"
          , HE.onClick \_ -> Just $ navigateAction AdminExhibitions
          ]
          [ HH.i
            [ css "fas fa-th"
            ]
            []
          , HH.text "Exhibitions" 
          ]
        , HH.a
          [ HE.onClick \_ -> Just $ navigateAction AdminTokens
          , css "menu-link"
          ]
          [ HH.i
            [ css "fas fa-coins"
            ]
            []
          , HH.text "Tokens" 
          ]
        , HH.a
          [ HE.onClick \_ -> Just $ navigateAction AdminTransactions
          , css "menu-link"
          ]
          [ HH.i
            [ css "fas fa-server"
            ]
            []
          , HH.text "Transactions" 
          ]
        ]
      , HH.div
        [ css "admin-content" ]
        [ slot ]
      ]
    ]

-- Container including header
headerContainer :: forall props act
                 . Boolean           -- Dark mode off/on
                ->  act              -- Dark mode action
                -> HH.HTML props act -- html
                -> HH.HTML props act
headerContainer dmStatus dmAction slot =
  HH.div
    [ css $ "wrapper dark-mode-" <> (show dmStatus) ]
    [ Style.stylesheet
    , header dmAction 
    , HH.div
      [ css "content" ]
      [ slot ]
    ]

component
  :: forall m r
   . MonadAff m
  => MonadAsk { userEnv :: UserEnv | r } m
  => ManageBlogPost m
  => ManageExhibition m
  => ManageItem m
  => ManageMedia m
  => ManageUser m
  => ManageTag m
  => ManageToken m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m 
component = H.mkComponent 
  { initialState: \_ -> 
    { route: Nothing
    , currentUser: Nothing
    , darkMode: true
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

    NavigateAction route -> navigate route

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route, currentUser } <- H.get 
      when (route /= Just dest) do
        case (isJust currentUser && dest `elem` [ Login ]) of
          false -> do
            H.modify_ _ { route = Just dest }
            w <- H.liftEffect window
            H.liftEffect $ CWindow.scrollTo 0 0 w
          _ -> pure unit
      pure (Just a)

  authorize :: Maybe User -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
  authorize mbUser html = case mbUser of
    Nothing ->
      HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ ->
      html
  
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route, currentUser, darkMode } = 
    case route of
      Just Home -> 
        headerContainer 
          darkMode 
          DarkModeToggle $ 
            HH.slot 
            (SProxy :: _ "home") 
            unit 
            Home.component 
            unit 
            absurd

      Just (BlogPost slug) ->
        headerContainer 
          darkMode 
          DarkModeToggle $ 
            HH.slot 
            (SProxy :: _ "blogPost") 
            unit 
            BlogPost.component 
            { slug: (Slug.generate slug) }
            absurd

      Just (Exhibition exId) ->
        headerContainer
          darkMode
          DarkModeToggle $
            HH.slot 
            (SProxy :: _ "exhibition")
            unit
            Exhibition.component
            { exhibitionId: exId }
            absurd

      Just Login ->
        HH.slot 
          (SProxy :: _ "login") 
          unit 
          Login.component 
          { redirect: true } 
          absurd

      Just (Tag tagId) -> 
        headerContainer 
          darkMode 
          DarkModeToggle $ 
            HH.slot 
            (SProxy :: _ "tag") 
            unit 
            TagPage.component 
            { tagId: tagId } 
            absurd

      -- Admin
      Just AdminHome ->
        adminContainer NavigateAction $
          HH.slot 
          (SProxy :: _ "adminHome") 
          unit 
          AdminHome.component 
          unit 
          absurd
          # authorize currentUser

      Just AdminBlogPosts ->
        adminContainer NavigateAction $
          HH.slot 
          (SProxy :: _ "adminBlogPosts")
          unit 
          AdminBlogPosts.component 
          unit 
          absurd
          # authorize currentUser

      Just (AdminBlogPost blogPostId) -> 
        adminContainer NavigateAction $
          HH.slot 
          (SProxy :: _ "adminBlogPost") 
          unit 
          AdminBlogPost.component 
          { blogPostId } 
          absurd
          # authorize currentUser

      Just AdminExhibitions ->
        adminContainer NavigateAction $
          HH.slot 
          (SProxy :: _ "adminExhibitions")
          unit
          AdminExhibitions.component
          unit
          absurd
          # authorize currentUser

      Just (AdminExhibition exId) ->
        adminContainer NavigateAction $
          HH.slot
          (SProxy :: _ "adminExhibition")
          unit
          AdminExhibition.component
          { exhibitionId: exId }
          absurd
          # authorize currentUser

      Just AdminTokens ->
        adminContainer NavigateAction $
          HH.slot
          (SProxy :: _ "adminTokens") 
          unit
          (AdminTokens.component)
          unit
          absurd
          # authorize currentUser

      Just (AdminToken tokenId) ->
        adminContainer NavigateAction $
          HH.slot
          (SProxy :: _ "adminToken")
          unit
          (AdminToken.component)
          { tokenId: tokenId }
          absurd
          # authorize currentUser

      Just AdminTransactions ->
        adminContainer NavigateAction $
          HH.slot
          (SProxy :: _ "adminTransactions")
          unit
          AdminTransactions.component
          unit
          absurd
          # authorize currentUser

      Nothing ->
        HH.div_ [ HH.text "Oh no! That page wasn't found." ]

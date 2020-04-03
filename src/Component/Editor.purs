module Component.Editor where

import Prelude
import Color                        as Color
import Control.Monad.Except         (runExcept)
import Control.Monad.Except.Trans   (runExceptT)
import Control.Monad.Error.Class    (class MonadError
                                    ,throwError)
import Data.Argonaut                (encodeJson, decodeJson)
import Data.Const                   (Const)
import Data.Either                  (Either(..))
import Data.Foldable                (fold, intercalate)
import Data.Maybe                   (Maybe(..))
import Data.Options                 (Options, (:=))
import Data.String.Utils            (startsWith)
import Data.Symbol                  (SProxy(..))
import Effect.Class                 (class MonadEffect)
import Effect.Class.Console         as Console
import Foreign.Generic              (genericDecodeJSON
                                    ,decodeJSON, encodeJSON)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Properties      as HP
import Foreign                      as Foreign
import Quill.Config                 as QConfig
import Quill.API.Content            as QContent
import Quill.API.Delta              as QDelta
import Quill.API.Events             as QEvents
import Quill.API.Formats            as QFormats
import Quill.API.Range              (Range)
import Quill.Editor                 as QEditor
import Quill.API.Source             as QSource
import Quill.API.HTML               as QHTML
import Web.HTML.HTMLElement         (HTMLElement)

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)

editorConfig :: Options QConfig.Config
editorConfig = fold
  [ QConfig.debug   := QConfig.DebugWarn
  , QConfig.theme   := QConfig.SnowTheme
  , QConfig.placeholder := "Write here!"
  , QConfig.formats := 
    [ QConfig.allow QFormats.bold
    , QConfig.allow QFormats.italic
    , QConfig.allow QFormats.underline
    , QConfig.allow QFormats.header
    , QConfig.allow QFormats.align
    , QConfig.allow QFormats.color
    , QConfig.allow QFormats.code
    , QConfig.allow QFormats.codeBlock
    , QConfig.allow QFormats.link
    ]
  ]

type Input =
  { content :: Maybe String
  }

type State = 
  { editor :: Maybe QEditor.Editor
  , content :: Maybe String
  }

data Action 
  = Initialize
  | Receive Input

type ChildSlots = ()

data Query a = GetText (String -> a)
             | GetHTMLText (String -> a)

initialState :: State
initialState = 
  { editor: Nothing
  , content: Nothing
  }

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      , handleQuery = handleQuery
      , receive = Just <<< Receive
      }
    }
  where
  handleAction = case _ of

    Initialize -> do
      state <- H.get
      element <- H.getHTMLElementRef (H.RefLabel "quill-editor")
      case element of
        Just e -> do
          editor <- QEditor.new editorConfig e
          H.modify_ _ { editor = Just editor }
        Nothing -> pure unit

    Receive input -> do
      state <- H.get
      case state.editor of
        Just editor -> do
          case input.content of
            Just cnt -> do
              case (startsWith "{" cnt) of
                true -> do
                  let 
                    contentDeltas :: Either Foreign.MultipleErrors QDelta.Ops
                    contentDeltas = runExcept $ decodeJSON cnt
                  case contentDeltas of
                    Right cnd -> do
                      ops <- runExceptT $ QContent.setContents cnd QSource.API editor
                      pure unit
                    Left err -> pure unit
                false -> do
                  ops <- runExceptT $ QContent.setText cnt QSource.API editor
                  pure unit
            Nothing -> pure unit
        Nothing -> pure unit

  handleQuery :: forall a
               . Query a 
              -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    GetText t -> do
      state <- H.get
      case state.editor of
        Just editor -> do
          l <- runExceptT $ QContent.getLength editor
          case l of
            Right le -> do
              content <- runExceptT $ QContent.getContents 
                {index: 0, length: Just le} editor
              case content of
                Right c  -> do
                  let 
                    encodedOps = encodeJSON c
                  Console.logShow encodedOps
                  Just <<< t <$> (pure encodedOps)
                Left err -> pure Nothing
            Left err -> pure Nothing
        Nothing -> pure Nothing

    GetHTMLText t -> do
      state <- H.get
      case state.editor of
        Just editor -> do
          htmlStr <- runExceptT $ QHTML.getHTMLString editor
          case htmlStr of
            Right str -> Just <<< t <$> (pure str)
            Left err -> pure Nothing
        Nothing -> pure Nothing

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [ HP.ref (H.RefLabel "quill-editor") ]
      []

renderMultipleErrors :: Foreign.MultipleErrors -> String
renderMultipleErrors = intercalate ", " <<< map Foreign.renderForeignError

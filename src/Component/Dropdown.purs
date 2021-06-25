module Component.Dropdown where

import Prelude

import Component.HTML.Utils (css, classes_, whenElem)
import Data.Array ((!!), mapWithIndex, length)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks (useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
import Select (SelectReturn(..), selectInput, useSelect)
import Select as S

type Slot = H.Slot (Const Void) Message

data Message
  = SelectionChanged (Maybe String) (Maybe String)

-- it is unnecessary to export your own input type, but doing so helps if you
-- would like to set some sensible defaults behind the scenes.
type Input =
  { items :: Array String
  , buttonLabel :: String
  }

component :: forall m
           . MonadAff m 
          => H.Component HH.HTML (Const Void) Input Message m
component = Hooks.component \tokens input -> Hooks.do
  selection /\ selectionId <- useState Nothing
  _ /\ inputRef <- Hooks.useRef input
  selectedIndexChanges <- useEvent

  Hooks.captures { input } Hooks.useTickEffect do
    liftEffect $ Ref.write input inputRef
    pure Nothing

  Hooks.captures {} Hooks.useTickEffect do
    pure $ Just $ do pure unit

  SelectReturn select <- do
    useSelect $ selectInput
      { getItemCount = pure (length input.items)
      , pushSelectedIdxChanged = selectedIndexChanges.push
      }

  useLifecycleEffect do
    void $ selectedIndexChanges.setCallback $ Just \_ ix -> do
      {items, buttonLabel } <- liftEffect $ Ref.read inputRef
      oldSelection <- Hooks.get selectionId
      let newSelection = items !! ix
      select.setVisibility S.Off
      logShow items
      Hooks.put selectionId newSelection
      Hooks.raise tokens.outputToken $ SelectionChanged oldSelection newSelection

    pure Nothing

  Hooks.pure $
    HH.div
      [ css "Dropdown" ]
      [ renderToggle select input.buttonLabel selection
      , renderContainer select input.items
      ]
  where
    renderToggle select buttonLabel selection =
      HH.button
        ( select.setToggleProps [ css "button Dropdown__toggle" ] )
        [ HH.text (fromMaybe buttonLabel selection) ]

    renderContainer select items =
      whenElem (select.visibility == S.On) \_ ->
        HH.div
          ( select.setContainerProps [ css "Dropdown__container" ] )
          ( mapWithIndex (renderItem select) items )

    renderItem select index item =
      HH.div
        ( select.setItemProps index
            [ classes_
                [ "Dropdown__item"
                , "Dropdown__item--highlighted"
                    # guard (select.highlightedIndex == Just index)
                ]
            ]
        )
        [ HH.text item ]

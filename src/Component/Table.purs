module Component.Table where 

import Prelude
import Effect.Class             (class MonadEffect)
import Data.Array               ((:))
import Data.Const               (Const(..))
import Data.Maybe               (Maybe(..))
import Halogen                  as H
import Halogen.HTML             as HH
import Halogen.HTML.Properties  as HP
import Halogen.HTML.Events      as HE
import Web.Event.Event          (preventDefault)
import Web.UIEvent.MouseEvent   (MouseEvent, toEvent)

import Component.HTML.Utils     (css)

type Query = Const Void

type Row = 
  { id          :: Int 
  , label       :: String 
  }

type Rows    = Array Row
type Headers = Array String

type State = 
  { rows    :: Rows
  , headers :: Headers
  }

type Input = State

data Output 
  = UpdateRow Row 
  | DeleteRow Row

data Action 
  = UpdateClicked MouseEvent Row
  | DeleteClicked MouseEvent Row
  | Receive Input 

component :: forall m 
           . MonadEffect m
          => H.Component HH.HTML Query Input Output m 
component = 
  H.mkComponent 
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
    where
    initialState :: Input -> State 
    initialState inp = 
      { rows: inp.rows
      , headers: inp.headers
      }

    handleAction = case _ of 
      UpdateClicked ev row -> do
        H.liftEffect $ preventDefault $ toEvent ev
        H.raise $ UpdateRow row 
      DeleteClicked ev row -> do
        H.liftEffect $ preventDefault $ toEvent ev
        H.raise $ DeleteRow row
      Receive inp       -> H.put inp

    tableHeader :: forall i p. String -> HH.HTML i p
    tableHeader col = 
      HH.th
        []
        [ HH.text col ]

    tableRow row =
      HH.tr
        []
        [ HH.td
          []
          [ HH.text row.label ]
        , HH.td
          []
          [ HH.a
            [ HE.onClick \e -> Just $ UpdateClicked e row 
            , HP.href "#"
            ]
            [ HH.i [ css "fas fa-pencil-alt" ] [] ]
          ]
        , HH.td
          []
          [ HH.a
            [ HE.onClick \e -> Just $ DeleteClicked e row 
            , HP.href "#"
            ]
            [ HH.i [ css "fas fa-trash" ] [] ]
          ]
        ]

    render state = 
      HH.table 
        [ css "full-width responsive-table" ]
        [ HH.thead
          []
          [ HH.tr
            []
            (state.headers <#> tableHeader)
          ]
        , HH.tbody
          []
          (state.rows <#> tableRow)
        ]


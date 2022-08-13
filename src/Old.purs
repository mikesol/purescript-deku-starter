module Old where

import Prelude

import Control.Alt ((<|>))
import Data.Array (null)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (oneOf, oneOfMap, for_)
import Data.Function (on)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Always (class Always)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dyn_, text, text_)
import Deku.Core (class Korok, Domable, bus, bussed, insert_, remove)
import Deku.DOM as D
import Deku.Do (useMemoized)
import Deku.Do as DekuDo
import Deku.Listeners (click)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, fold, fromEvent, keepLatest, toEvent)
import Paraglider.Rx (doOnNext)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

data UpwardsCmd = EmitMe Int String | DeleteMe Int
type DownwardsCmd = Array Int

dynamicPart
  :: forall s m lock payload
   . Korok s m
  => (UpwardsCmd -> Effect Unit)
  -> AnEvent m DownwardsCmd
  -> Domable m lock payload
dynamicPart upCmdPush downCmdEv = D.div_
  [ dyn_ D.div $ (keepLatest (map (oneOfMap pure) downCmdEv))
      <#> \index -> keepLatest $ bus \setRemove rm ->
        pure
          ( insert_
              ( bussed \setText text -> D.div_
                  [ D.span_ [ text_ $ "Model id: " <> show index ]
                  , D.button
                      ( click $ pure $ do
                          upCmdPush (DeleteMe index)
                          setRemove unit
                      )
                      [ text_ "-" ]
                  , D.input
                      ( oneOf
                          [ pure $ D.OnInput := cb \e -> for_
                              (target e >>= fromEventTarget)
                              (value >=> setText)
                          ]
                      )
                      []
                  , D.button
                      ( click $ text <#> \t -> do
                          upCmdPush (EmitMe index t)
                      )
                      [ text_ "Emit->" ]
                  ]
              )
          ) <|> (rm $> remove)
  ]

type IndexedEmission = { value :: String, index :: Int }
type SourceId = Int
type SourcesStore = Map SourceId (Array IndexedEmission)
type AddSourcesCmd = Array SourceId

type MyState =
  { store :: SourcesStore
  , addSourcesCmd :: AddSourcesCmd
  , lastEmissionIndex :: Int
  }

type StateMutationType = MyState -> MyState

convertUpwardsActionToState :: UpwardsCmd -> StateMutationType
convertUpwardsActionToState = case _ of
  EmitMe id value -> \{store, lastEmissionIndex} ->
    { store: ( Map.alter
        ( case _ of
            Nothing -> Just []
            Just previous -> Just (Array.cons ({ value, index: lastEmissionIndex }) previous)
        )
        id
        store
    )
    , addSourcesCmd: []
    , lastEmissionIndex: lastEmissionIndex + 1
    }
  DeleteMe id -> \{store, lastEmissionIndex} ->
    {store: (Map.delete id store), addSourcesCmd: [], lastEmissionIndex}

initialState :: MyState
initialState =
  { store: Map.fromFoldable [ 0 /\ [], 1 /\ [] ]
  , addSourcesCmd: [ 0, 1 ]
  , lastEmissionIndex: 0
  }

nut :: ∀ s m l p. Korok s m => Domable m l p
nut = DekuDo.do
  setCurrentId /\ currentIdEv <- useMemoized \e -> e <|> pure 2

  (modifyState :: StateMutationType -> Effect Unit)
    /\ (stateEv :: AnEvent m MyState) <- useMemoized \e ->
      (fold ($) (e <|> pure identity) (initialState))

  let addItem currentId = do
        modifyState $ \s@{store} ->
          s {store = Map.insert currentId [] store, addSourcesCmd = [ currentId ]}
        setCurrentId (currentId + 1)

  D.div_
    [ dynamicPart (lcmap convertUpwardsActionToState modifyState) (map (_.addSourcesCmd) stateEv)
    , D.div_
        [ D.button
            ( click (currentIdEv <#> addItem))
            [ text_ "+" ]
        ]
    , D.div_ [ text (stateEv <#> \s -> "Current models: " <> show (Map.keys (s.store))) ]
    , D.div_
        [ text $ map (_.store) stateEv <#> printStoreEmissions
        ]
    ]

printStoreEmissions :: SourcesStore -> String
printStoreEmissions s = do
  let
    l = Map.toUnfoldable s
    partitioned = l # partitionMap \(id /\ emissions) -> case emissions of
      [] -> Left id
      x -> Right x

    idsMissingEmission :: Array Int
    idsMissingEmission = partitioned.left

    allEmissions :: Array IndexedEmission
    allEmissions = join (partitioned.right)

    orderedValues :: Array String
    orderedValues = map (_.value) $ Array.sortBy (compare `on` (_.index)) allEmissions

  if null idsMissingEmission then
    "Emits in order from earliest to latest: " <> show orderedValues
  else
    "Waiting for these models before emitting: " <> (show idsMissingEmission)

main :: Effect Unit
main = runInBody nut

logEmission
  :: forall s m a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => (a -> String)
  -> AnEvent m a
  -> AnEvent m a
logEmission onEmit e = fromEvent $ doOnNext (\a -> log $ onEmit a) $ toEvent e
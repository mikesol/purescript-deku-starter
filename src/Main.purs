module Main where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST)
import Data.Array (null)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Filterable (filter, filterMap, partitionMap)
import Data.Foldable (for_, oneOf)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Always (class Always)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dyn_, text, text_)
import Deku.Core (class Korok, Domable, bussedUncurried, insert_, remove)
import Deku.DOM as D
import Deku.Do as DekuDo
import Deku.Listeners (click)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, count, fold, fromEvent, memoize, toEvent)
import Paraglider.Rx (doOnNext)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)

type SourceId = Int

type Mail a b = { address :: a, payload :: b }
type MailPush a b = Mail a b -> Effect Unit
type MailEvent m a b = AnEvent m (Mail a b)
type MailHub m a b = MailPush a b /\ MailEvent m a b

stamp :: ∀ a b. a -> b -> Mail a b
stamp address payload = { address, payload }

watchAddress :: ∀ m a b. Applicative m => Eq a => a -> MailEvent m a b -> AnEvent m b
watchAddress address' = filterMap \{address, payload} ->
  if address' == address then Just payload else Nothing

data Action = EmitMe String | DeleteMe | AddMe
derive instance genericAction :: Generic Action _
instance eqAction :: Eq Action where eq = genericEq

initialActions :: ∀ s m. MonadST s m => MailEvent m SourceId Action
initialActions = (pure $ stamp 0 AddMe) <|> (pure $ stamp 1 AddMe)

dynamicPart :: ∀ s m l p . Korok s m => MailHub m SourceId Action -> Domable m l p
dynamicPart (mailPush /\ mailEv) = do
  D.div_
    [ dyn_ D.div $ mkRowEv <$> serializedIdsEv
    , D.div_
        [ D.button
            ( click ((\id -> mailPush $ stamp id AddMe) <$> currentIdEv))
            [ text_ "+" ]
        ]
    ]
  where
    serializedIdsEv :: AnEvent m SourceId
    serializedIdsEv = filterMap
      (\{address, payload} -> if payload == AddMe then Just address else Nothing)
      mailEv

    currentIdEv  :: AnEvent m Int
    currentIdEv = count $ filter (\{payload} -> payload == AddMe) mailEv

    rowUi :: Int -> Domable m l p
    rowUi id = DekuDo.do
      let pushAddressed = mailPush <<< stamp id
      setText /\ text <- bussedUncurried
      D.div_
          [ D.span_ [ text_ $ "Model id: " <> show id ]
          , D.button
              ( click $ pure $ pushAddressed DeleteMe)
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
                  pushAddressed (EmitMe t)
              )
              [ text_ "Emit->" ]
          ]

    mkRowEv :: Int -> AnEvent m _ -- dyn cmds
    mkRowEv id = do
      let myDeletionEv = filterMap (\a -> if a == DeleteMe then Just remove else Nothing)
            $ watchAddress id mailEv
      pure (insert_ $ rowUi id) <|> myDeletionEv

type IndexedEmission = { value :: String, index :: Int }
type SourcesStore = Map SourceId (Array IndexedEmission)

type MyState =
  { store :: SourcesStore
  , lastEmissionIndex :: Int
  }

type StateMutator = MyState -> MyState

convertUpwardsActionToState :: Mail SourceId Action -> StateMutator
convertUpwardsActionToState {address: id, payload}= case payload of
  AddMe -> \{store, lastEmissionIndex} ->
    { store: Map.insert id [] store
    , lastEmissionIndex
    }
  EmitMe value -> \{store, lastEmissionIndex} ->
    { store: ( Map.alter
        ( case _ of
            Nothing -> Just []
            Just previous -> Just (Array.cons ({ value, index: lastEmissionIndex }) previous)
        )
        id
        store
    )
    , lastEmissionIndex: lastEmissionIndex + 1
    }
  DeleteMe -> \{store, lastEmissionIndex} ->
    {store: (Map.delete id store), lastEmissionIndex}

initialState :: MyState
initialState =
  { store: Map.empty
  , lastEmissionIndex: 0
  }

nut :: ∀ s m l p. Korok s m => Domable m l p
nut = DekuDo.do
  childPush /\ childEv' <- bussedUncurried
  let childEv = childEv' <|> initialActions
  let stateFEv = convertUpwardsActionToState <$> childEv

  stateEv <- envy <<< memoize do
    (fold ($) (stateFEv <|> pure identity) (initialState))

  D.div_
    [ dynamicPart (childPush /\ childEv)
    , D.div_ [ text (stateEv <#> \s -> "Current imodels: " <> show (Map.keys (s.store))) ]
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

useChild
  :: forall s m lock logic obj a r
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => ((a -> Effect Unit) /\ AnEvent m a -> { ent :: Bolson.Entity logic obj m lock | r })
  -> ({ ent :: Bolson.Entity logic obj m lock | r } -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
useChild f0 f1 = bussedUncurried (\io -> f1 (f0 io))

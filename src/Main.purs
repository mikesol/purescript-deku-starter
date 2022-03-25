module Main where

import Prelude

import Deku.Control.Functions (freeze, u)
import Deku.Graph.DOM.Shorthand as S
import Effect (Effect)
import Deku.Toplevel ((🚀))

main :: Effect Unit
main = (const $ u $ S.text "Hello world") 🚀 freeze

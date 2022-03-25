module Main where

import Prelude

import Deku.Control.Functions (freeze, u)
import Deku.Pursx ((~!))
import Deku.Toplevel ((🚀))
import Effect (Effect)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main =
  (\_ -> u $ (Proxy :: _ "<div>Hello world</div>") ~! {})
    🚀 freeze

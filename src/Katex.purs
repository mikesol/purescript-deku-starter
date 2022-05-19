module Katex where

import Prelude

import Effect (Effect)
import Web.DOM (Element)

foreign import render :: String -> Element -> Effect Unit
module Main where

import Prelude

import Deku.Attribute ((:=))
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event.Class (bang)
import Katex as Katex

main :: Effect Unit
main = runInBody
  ( D.span
      (bang (D.Self := Katex.render "c = \\pm\\sqrt{a^2 + b^2}"))
      []
  )
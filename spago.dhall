{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "prelude"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
}

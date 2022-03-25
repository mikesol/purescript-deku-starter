{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "deku-toplevel"
  , "effect"
  , "event"
  , "foldable-traversable"
  , "prelude"
  , "web-html"
  ]
, packages = ./packages.dhall
}

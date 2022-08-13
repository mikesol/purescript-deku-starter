{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "arrays"
  , "bolson"
  , "console"
  , "control"
  , "deku"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "hyrule"
  , "hyrule-paraglider"
  , "maybe"
  , "monoid-extras"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "routing"
  , "routing-duplex"
  , "st"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
}

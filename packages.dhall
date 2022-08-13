let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/2d448b3cb24b7c7a1506a4e1ffaf0047af8a3304/src/packages.dhall
        sha256:1046637437530f8ca29fe960170c874250fc561d96eddac7242137e096e0e59c

let overrides =
      { hyrule-paraglider = ../hyrule-paraglider/spago.dhall as Location
      , deku =
        { dependencies =
          [ "arrays"
          , "bolson"
          , "control"
          , "effect"
          , "fast-vect"
          , "filterable"
          , "foldable-traversable"
          , "foreign-object"
          , "heterogeneous"
          , "hyrule"
          , "maybe"
          , "monoid-extras"
          , "newtype"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "quickcheck"
          , "record"
          , "refs"
          , "safe-coerce"
          , "st"
          , "strings"
          , "transformers"
          , "unsafe-coerce"
          , "web-dom"
          , "web-events"
          , "web-html"
          ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        --, version = "always-tmp"
        , version = "v0.4.15"
        }
      }

in  upstream // overrides

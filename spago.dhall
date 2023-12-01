{ name = "bus-rider"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "exceptions"
  , "fetch"
  , "js-fetch"
  , "js-promise-aff"
  , "maybe"
  , "node-fs"
  , "node-process"
  , "partial"
  , "prelude"
  , "string-parsers"
  , "transformers"
  , "web-encoding"
  , "web-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/purs/*.purs", "test/**/*.purs" ]
}

# CHANGELOG

- 0.4.5.0 (2018-05-08)
    * Tweak saturation and lightness for readability (by @nqac)
    * Bump `aeson` dependency to 1.3
    * Bump `js-jquery` dependency to 3.3

- 0.4.4.0
    * Add a flag to embed the datafiles (by Nikolay Yakimov)
    * Allow passing in an output path (by Nikolay Yakimov)
    * Use HSL instead of RGB for random color generation (by David
      Luposchainsky)

- 0.4.3.0
    * Add `--version` flag
    * Bump `aeson` dependency to 1.2

- 0.4.2.2
    * Bump `js-jquery` dependency to 3.2

- 0.4.2.1
    * Bump `ghc-prof` dependency to 1.4

- 0.4.2.0
    * Use the new `ghc-prof` instead of our own hacky prof parser
    * Bump `aeson` dependency to 1.1
    * Bump `vector` dependency to 0.12

- 0.4.1.0
    * Format numbers in "Details" to have 3 decimal places
    * Move name below controls in "Details"
    * Use the `js-jquery` library instead of just including it
    * Upgrade to jQuery 3.1

- 0.4.0.0
    * Always show time/alloc relative to MAIN as well as relative to the current
      root node
    * Replace help button by link to homepage
    * Never include (0 time, 0 alloc) nodes

- 0.3.0.3
    * Bump `aeson` dependency

- 0.3.0.2
    * Bump `aeson` dependency

- 0.3.0.1
    * Bump base version, add example to repo

- 0.3.0.0
    * Simplify the tree, creating actual (indiv) nodes where necessary from the
      Haskell code

- 0.2.0.2
    * Bump `aeson` and `vector` dependencies

- 0.2.0.1
    * Bump `aeson` and `attoparsec` dependencies

- 0.2.0.0
    * Show individual time/alloc as separate nodes

- 0.1.2.2
    * Loosen `text` dependency

- 0.1.2.1
    * Loosen `text` dependency

- 0.1.2.0
    * Fix resize/redraw bug for Firefox/Linux combo

- 0.1.1.0
    * Ignore extra information in `.prof` file

- 0.1.0.1
    * Loosen `attoparsec` dependency

- 0.1.0.0
    * Initial release

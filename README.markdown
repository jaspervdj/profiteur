profiteur
=========

Profiteur is a visualiser for GHC `.prof` files.

Installation
------------

    cabal install profiteur

Usage
-----

    ghc --make -auto-all -prof your-program.hs
    ./your-program +RTS -p -RTS
    profiteur your-program.prof

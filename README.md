elm-playground
==============

You will need cabal and the latest haskell (I'm on 7.8.3).

1. Install Haskell then Cabal with brew or by other means
2. Run these 4 lines to get elm setup locally.

cabal update
cabal install cabal-install
cabal install -j elm-compiler-0.14 elm-package-0.2 elm-make-0.1
cabal install -j elm-repl-0.4 elm-reactor-0.2.0.1

This presentation uses [Trex](https://github.com/pittma/trex). To view it, you
will need Trex installed, which depends on Haskell's Stack package manager. The
instructions below assume you do not have a working Haskell or Stack
installation. If you already do, skip to the `git clone` step.

```sh
# install ghcup
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# install ghc and stack.
# select GHC 9.6.7 and the recommended version of Stack.
$ ghcup tui
# clone the repo
$ git clone git@github.com:pittma/trex.git
# install trex
$ cd trex
$ stack install
```

Now, from this directory, run `trex serve`. The presentation will be available
at `http://localhost:8080/deck.html`.

**NOTE:** once the presentation has been compiled once, it is self-contained at
`_site/deck.html`. If no changes need to be made, you can simply open that file
in your browser. `trex serve` watches slides for changes and updates the site,
but it may not be needed if your intention is only to view the presentation.

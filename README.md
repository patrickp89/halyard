# halyard
An offline, stand-alone regular expression tester.

## ...but why?
If you want to test regular expressions on any sensitive data you might not trust one of those ("popular"?) online RegEx testing tools. Halyard instead runs totally offline, is open-source and meant to be used stand-alone (i.e. its not a plugin for a browser that you do not use.) Its only dependency are the GTK+ libraries.

## How to build it?
### Dependencies
Install the GTK+ and GObject Introspection developer packages:
```bash
$ apt-get install libgtk-3-dev libgirepository1.0-dev
```

### With Cabal
Install [Cabal](https://www.haskell.org/cabal/) and build it:
```bash
$ curl https://gitlab.haskell.org/haskell/ghcup/raw/master/bootstrap-haskell -sSf | sh
$ cabal install haskell-gi gi-gtk
$ cabal build
```

### With Stack
You can also build halyard via [Stack](https://docs.haskellstack.org/en/stable/README/)!
Install Stack, initialize the project once, and build:
```bash
$ curl -sSL https://get.haskellstack.org/ | sh
$ stack init
$ stack build
```

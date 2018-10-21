# Dunai-core

[![Build Status](https://travis-ci.org/turion/dunai-core.svg?branch=develop)](https://travis-ci.org/turion/dunai-core)
[![Version on Hackage](https://img.shields.io/hackage/v/dunai-core.svg)](https://hackage.haskell.org/package/dunai-core)

This repository implements a generalized version of reactive programming, on
top of which other variants like Yampa, Classic FRP and Reactive Values can
be implemented.
It is a fork of [`dunai`](https://github.com/ivanperez-keera/dunai) and just contains the core library,
with a few minor modifications.
The main reason this fork exists is as a dependency for [`rhine`](https://github.com/turion/rhine),
where support for GHC 8.6 and current stack nightly is wanted.
It might disappear in the future.
If in doubt, you should choose `dunai` over `dunai-core`.

# Installation

## Cabal

```
$ cabal sandbox init         # Optional, but recommended
$ cabal update
$ cabal install dunai
```

## stack

```
$ git clone https://github.com/turion/dunai-core
$ cd dunai-core
$ stack build
```

## Dependencies

Dunai currently support GHC versions 8.2.1 to 8.6.

# Examples

To test Dunai:

- Use `embed :: MSF m a b -> [a] -> m [b]` to collect
  a list with the results.

- Use `embed_ :: MSF m a () -> [a] -> m ()` to perform side effects without
  collecting the results.

- Use `reactimate :: MSF m () () -> m ()` when data is collected/provided by the
  MSF itself.

```haskell
ghci> import Data.MonadicStreamFunction
ghci> embed (arr (+1)) [1,2,3,4,5]
[2,3,4,5,6]
ghci> embed_ (arr (+1) >>> liftS print) [1,2,3,4,5]
2
3
4
5
6
ghci> reactimate (arrM_ getLine >>> arr reverse >>> liftS putStrLn)
Hello
olleH
Haskell is awesome
emosewa si lleksaH
^C
```

# Further reading

The best introduction to the fundamentals of Monadic Stream Functions is:

- [Functional Reactive Programming, Refactored](https://dl.acm.org/authorize?N34896) ([official ACM page](http://dl.acm.org/citation.cfm?id=2976010)) ([mirror](http://www.cs.nott.ac.uk/~psxip1/))

The following papers are also related to MSFs:

- [Back to the Future: time travel in FRP](http://dl.acm.org/citation.cfm?id=3122957) ([mirror](http://www.cs.nott.ac.uk/~psxip1/))

- [Testing and Debugging Functional Reactive Programming](http://dl.acm.org/citation.cfm?id=3110246)

# About the name

Dunai (aka. Danube, or Дунай) is one of the main rivers in Europe, originating
in Germany and touching Austria, Slovakia, Hungary, Croatia, Serbia, Romania,
Bulgaria, Moldova and Ukraine.

Other FRP libraries, like Yampa, are named after rivers.  Dunai has been chosen
due to the authors' relation with some of the countries it passes through, and
knowing that this library has helped unite otherwise very different people from
different backgrounds.

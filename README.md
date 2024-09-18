## gnuplot-dsl: An eDSL for gnuplot functions in Haskell

This project was inspired by the [gnuplot haskell package](https://hackage.haskell.org/package/gnuplot) and aims to fix some of the issues I've had while using it.

### Installation

**This package is not yet available on Hackage**. To install, clone the package locally and build it with Cabal or Nix:
```sh
# With Cabal..
cabal build
# .. or with Nix
nix build .#
```
If built with nix, it will use packages available in the nixpkgs repository as dependencies instead of those available on Hackage.

### Usage

Simply, open up a GHCi session and import "Graphics.Gnuplot.DSL" and you're ready to go
```hs
plot (\x -> sin (pi * x))
plot (\x y -> x ** 4 + y ** 4)
plot [\x -> x ** 3 - x ** 2, exp]
plot $ sin `withStyle` FilledCurve
```
To restrict the domain of a function, use the "withXDomain", "withYDomain" and "withZDomain":
```hs
plot $ cos `withXDomain` (0,3) `withYDomain` (Auto, 1)
```
The library is a bit limited in its other aspects so far, but it allows setting arbitrary gnuplot variables and printing the generated gnuplot command. Use the "plotWithInit" function to do these:
```hs
plotWithInit [Set "key" "outside", Toggle "title"] True log
```

External data and local function definitions are not yet supported.

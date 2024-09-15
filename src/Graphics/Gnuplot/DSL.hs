{-|
Module      : Graphics.Gnuplot.DSL
Copyright   : (c) Márton Petes, 2024
License     : MIT
Maintainer  : tx0lwm@inf.elte.hu

The primary module you should be working with when rendering simple functions, see the individual modules for further detail.
-}
module Graphics.Gnuplot.DSL (module Gnuplot) where

import Graphics.Gnuplot.DSL.Drawable as Gnuplot (
  GDrawable,
  GnuplotDrawable,
  Style (..),
  RangeComp (..),
  withNOOP,
  withStyle,
  withXDomain,
  withYDomain,
  withZDomain,
 )

import Graphics.Gnuplot.DSL.Expr as Gnuplot (
  GExpr,
  func,
  gceil,
  gfloor,
  ground,
 )

import Graphics.Gnuplot.DSL.Process as Gnuplot (
  gnuplotExec,
  plot,
  plotDebug,
  plotWithInit,
 )

import Graphics.Gnuplot.DSL.Command as Gnuplot (
  GCommand (..),
 )

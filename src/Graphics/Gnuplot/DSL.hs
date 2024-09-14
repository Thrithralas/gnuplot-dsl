module Graphics.Gnuplot.DSL (module Gnuplot) where

import Graphics.Gnuplot.DSL.Drawable as Gnuplot (
  GDrawable,
  GnuplotDrawable,
  Style (..),
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

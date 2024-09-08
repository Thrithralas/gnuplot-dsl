module Graphics.Gnuplot.DSL (module Gnuplot) where

import Graphics.Gnuplot.DSL.Drawable as Gnuplot (
  GDrawable,
  Style(..),
  GnuplotDrawable,
  withXDomain,
  withYDomain,
  withZDomain,
  withStyle
  )

import Graphics.Gnuplot.DSL.Expr as Gnuplot (
  GExpr,
  func,
  ground,
  gfloor,
  gceil
  )

import Graphics.Gnuplot.DSL.Process as Gnuplot (plot)

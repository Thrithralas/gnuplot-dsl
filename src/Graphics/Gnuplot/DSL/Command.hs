{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Graphics.Gnuplot.DSL.Command
Copyright   : (c) Márton Petes, 2024
License     : MIT
Maintainer  : tx0lwm@inf.elte.hu

Module for assembling actual commands which can be interpreted by gnuplot, from 'Graphics.Gnuplot.DSL.Expr.GExpr's.
-}
module Graphics.Gnuplot.DSL.Command (GCommand (..)) where

import Data.Char
import Data.List
import Graphics.Gnuplot.DSL.Drawable

type Dom = Maybe (RangeComp Double, RangeComp Double)

-- | Commands which can be sent to gnuplot.
data GCommand
  = Plot {domX :: Dom, domY :: Dom, functions :: [GDrawable]} -- ^ The plot command in gnuplot for drawing 2d functions.
  | SPlot {domX :: Dom, domY :: Dom, domZ :: Dom, functions :: [GDrawable]} -- ^ The splot command in gnuplot for drawing 3d functions.
  | Set String String -- ^ The set command for any values. If you want to set something like the title, you'll have to add another pair of quotation marks.
  | Toggle String -- ^ The set command, but without a value to set to.

  -- TODO: Add some structure to options


instance Show GCommand where
  show (Plot domX domY gds) =
    "plot "
      ++ maybe "[:] " (\(x, y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domX
      ++ maybe "[:] " (\(x, y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domY
      ++ intercalate ", " (map (\MkGDrawable{expr, style, ..} -> show expr ++ " w " ++ map toLower (show style)) gds)
  show (SPlot domX domY domZ gds) =
    "splot "
      ++ maybe "[:] " (\(x, y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domX
      ++ maybe "[:] " (\(x, y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domY
      ++ maybe "[:] " (\(x, y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domZ
      ++ intercalate ", " (map (\MkGDrawable{expr, style, ..} -> show expr ++ " w " ++ map toLower (show style)) gds)
  show (Set var val) = "set " ++ var ++ " " ++ val
  show (Toggle var) = "set " ++ var

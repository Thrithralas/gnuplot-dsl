{-# LANGUAGE RecordWildCards #-}

module Graphics.Gnuplot.DSL.Command (GCommand (..)) where

import Data.Char
import Data.List
import Graphics.Gnuplot.DSL.Drawable

type Dom = Maybe (RangeComp Double, RangeComp Double)

data GCommand
  = Plot {domX :: Dom, domY :: Dom, functions :: [GDrawable]}
  | SPlot {domX :: Dom, domY :: Dom, domZ :: Dom, functions :: [GDrawable]}
  | Set String String -- TODO: Add some structure to options
  | Toggle String

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

{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Graphics.Gnuplot.DSL.Expr
Copyright   : (c) Márton Petes, 2024
License     : MIT
Maintainer  : tx0lwm@inf.elte.hu

The definition of the eDSL's syntax. The constructors of the eDSL should rarily be used outside of this module. If you need to invoke a custom function, use the 'func' function.
-}
module Graphics.Gnuplot.DSL.Expr where

import Data.Data
import Data.Default
import GHC.Generics

-- | An expression in gnuplot, the eDSL the project keeps mentioning. The type supports arbitrary free variables which is used to convert from the lambda functions passed to 'plot'. When a function is flattened into a single 'GExpr', it's passed a parameter to turn it into a single expression with free variables. This is later converted to a string and sent to gnuplot.
data GExpr a
  = Lit a
  | Var String
  | GExpr a :+ GExpr a
  | GExpr a :- GExpr a
  | GExpr a :* GExpr a
  | GExpr a :/ GExpr a
  | GExpr a :** GExpr a
  | String :$ GExpr a
  deriving (Generic, Typeable, Data, Functor)

instance Default (GExpr a) where
  def = Var "x"

-- | Call a function by name that is not in this library, but available in gnuplot (like gamma).
func :: String -> GExpr a -> GExpr a
func = (:$)

instance Num a => Num (GExpr a) where
  (+) = (:+)
  (*) = (:*)
  (-) = (:-)
  abs = func "abs"
  signum = func "sgn"
  fromInteger = Lit . fromInteger

instance Fractional a => Fractional (GExpr a) where
  fromRational = Lit . fromRational
  (/) = (:/)

instance Floating a => Floating (GExpr a) where
  (**) = (:**)
  sqrt = func "sqrt"
  pi = Var "pi"
  exp = func "exp"
  log = func "log"
  sin = func "sin"
  cos = func "cos"
  tan = func "tan"
  asin = func "asin"
  acos = func "acos"
  atan = func "atan"
  sinh = func "sinh"
  cosh = func "cosh"
  tanh = func "tanh"
  asinh = func "asinh"
  acosh = func "acosh"
  atanh = func "atanh"


ground, gceil, gfloor :: GExpr a -> GExpr a
-- | Rounding, but for gnuplot expressions.
ground = func "round"
-- | Ceiling, but for gnuplot expressions.
gceil = func "ceil"
-- | Floor, but for gnuplot expressions.
gfloor = func "floor"

instance Show a => Show (GExpr a) where
  showsPrec p e0 = case e0 of
    Lit a -> showParen (p > 10) $ showsPrec 11 a
    Var s -> showParen (p > 10) $ showString s
    l :+ r -> showParen (p > 6) $ showsPrec 6 l . showString " + " . showsPrec 7 r
    l :- r -> showParen (p > 6) $ showsPrec 6 l . showString " - " . showsPrec 7 r
    l :* r -> showParen (p > 7) $ showsPrec 7 l . showString " * " . showsPrec 8 r
    l :/ r -> showParen (p > 7) $ showsPrec 7 l . showString " / " . showsPrec 8 r
    l :** r -> showParen (p > 8) $ showsPrec 9 l . showString " ** " . showsPrec 8 r
    f :$ r -> showParen (p > 10) $ showString f . showsPrec 11 r

{-# LANGUAGE TypeFamilies #-}
module Graphics.Gnuplot.DSL where

import Data.Data
import GHC.Generics


data GExpr a
  = Lit a
  | Var String
  | GExpr a :+ GExpr a
  | GExpr a :- GExpr a
  | GExpr a :* GExpr a
  | GExpr a :/ GExpr a
  | String :$ GExpr a
  deriving (Generic, Typeable, Data)

class GnuplotExpression a where
  type GExprType a
  toGExpr :: a -> GExpr (GExprType a)

func :: String -> GExpr a -> GExpr a
func = (:$)

gfloor, gceil, ground :: GExpr a -> GExpr a
gfloor = func "floor"
gceil = func "ceil"
ground = func "round"

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

instance GnuplotExpression (GExpr a) where
  type instance GExprType (GExpr a) = a
  toGExpr = id

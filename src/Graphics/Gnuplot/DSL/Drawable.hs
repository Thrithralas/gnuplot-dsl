{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphics.Gnuplot.DSL.Drawable (
  Style (..),
  GDrawable (..),
  GnuplotDrawable (..),
  RangeComp (..),
  withXDomain,
  withYDomain,
  withZDomain,
  withStyle,
  withNOOP,
)
where

import Control.Applicative
import Data.Char
import Data.Default
import Data.List
import GHC.Exts
import GHC.Generics
import Graphics.Gnuplot.DSL.Expr

data Style
  = Lines
  | Impulses
  | Points
  | Dots
  | FilledCurve
  | FilledCurves
  | CandleSticks
  | XYErrorBars
  | BoxXYErr
  | XErr
  | YErr
  deriving (Show, Generic)

data RangeComp a
  = RLit a
  | Auto
  deriving (Generic)

instance Show a => Show (RangeComp a) where
  show (RLit a) = show a
  show Auto = "*"

instance Default (RangeComp a) where
  def = Auto

instance Default Style where
  def = Lines

data GDrawable = MkGDrawable
  { expr :: GExpr Double,
    xDomain :: Maybe (RangeComp Double, RangeComp Double),
    yDomain :: Maybe (RangeComp Double, RangeComp Double),
    zDomain :: Maybe (RangeComp Double, RangeComp Double),
    style :: Style,
    threeDimensions :: Bool
  }
  deriving (Show, Generic)

instance Default GDrawable where
  def = MkGDrawable def def def def def False

class GnuplotDrawable c where
  toGDrawables :: (GDrawable -> GDrawable) -> c -> [GDrawable]

instance GnuplotDrawable GDrawable where
  toGDrawables f = singleton . f

instance GnuplotDrawable g => GnuplotDrawable [g] where
  toGDrawables f = concatMap (toGDrawables f)

instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p) where
  toGDrawables g f = [g $ def{expr = fmap (fromRational . toRational) $ f $ Var "x"}]

instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p -> p) where
  toGDrawables g f = [g $ def{expr = fromRational . toRational <$> f (Var "x") (Var "y"), threeDimensions = True}]

withXDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withXDomain g t = toGDrawables (\gd -> gd{xDomain = Just t}) g

withYDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withYDomain g t = toGDrawables (\gd -> gd{yDomain = Just t}) g

withZDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withZDomain g t = toGDrawables (\gd -> gd{zDomain = Just t}) g

withStyle :: GnuplotDrawable g => g -> Style -> [GDrawable]
withStyle g s = toGDrawables (\gd -> gd{style = s}) g

withNOOP :: GnuplotDrawable g => g -> [GDrawable]
withNOOP = toGDrawables id

infixl 4 `withXDomain`, `withYDomain`, `withZDomain`, `withStyle`

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

-- | The various styles gnuplot functions can be represented in. Noe that some of these are exclusive to data files which are not yet implemented.
data Style
  = Lines
  | Impulses
  | Points
  | Dots
  | FilledCurve
  | CandleSticks
  | XYErrorBars
  | BoxXYErr
  | XErr
  | YErr
  deriving (Show, Generic)

-- | An expression representing a gnuplot range. Either a literal or auto, which lets gnuplot decide.
data RangeComp a
  = RLit a
  | Auto
  deriving (Generic)

instance Num a => Num (RangeComp a) where
  RLit a + RLit b = RLit (a + b)
  _ + _ = Auto
  RLit a * RLit b = RLit (a * b)
  _ * _ = Auto
  RLit a - RLit b = RLit (a - b)
  _ - _ = Auto
  signum (RLit a) = RLit (signum a)
  signum Auto = Auto
  abs (RLit a) = RLit (abs a)
  abs Auto = Auto
  fromInteger = RLit . fromInteger

instance Fractional a => Fractional (RangeComp a) where
  RLit a / RLit b = RLit (a / b)
  _ / _ = Auto
  fromRational = RLit . fromRational

instance Show a => Show (RangeComp a) where
  show (RLit a) = show a
  show Auto = "*"

instance Default (RangeComp a) where
  def = Auto

instance Default Style where
  def = Lines

-- | A gnuplot drawable function with all its associated data.
data GDrawable = MkGDrawable
  { expr :: GExpr Double, -- ^ The expression of the function body.
    xDomain :: Maybe (RangeComp Double, RangeComp Double), -- ^ The domain of a 2D function
    yDomain :: Maybe (RangeComp Double, RangeComp Double), -- ^ The domain of a 3D function or the codomain of a 2D one
    zDomain :: Maybe (RangeComp Double, RangeComp Double), -- ^ The codomain of a 3D function
    style :: Style, -- ^ The associated style of the function
    threeDimensions :: Bool -- ^ Whether this function should be rendered in three dimensions or not
  }
  deriving (Show, Generic)

instance Default GDrawable where
  def = MkGDrawable def def def def def False

-- | A class of things that can be drawn via 'plot'.
class GnuplotDrawable c where
  -- | A function to convert a drawable into a list of gnuplot functions. The first parameter of this function is a transformation applied to all generated elements and is only used by the with-functions.
  toGDrawables :: (GDrawable -> GDrawable) -> c -> [GDrawable]

instance GnuplotDrawable GDrawable where
  toGDrawables f = singleton . f

instance GnuplotDrawable g => GnuplotDrawable [g] where
  toGDrawables f = concatMap (toGDrawables f)

-- | NOTE: We have to use 'UndecidableInstances', because this way the typechecker doesn't have to guess type variables
instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p) where
  toGDrawables g f = [g $ def{expr = fmap (fromRational . toRational) $ f $ Var "x"}]

instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p -> p) where
  toGDrawables g f = [g $ def{expr = fromRational . toRational <$> f (Var "x") (Var "y"), threeDimensions = True}]

-- | Change the X Domain of a function. This makes lists a bit complicated, see 'withNOOP'.
withXDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withXDomain g t = toGDrawables (\gd -> gd{xDomain = Just t}) g

-- | Change the Y Domain of a function. This makes lists a bit complicated, see 'withNOOP'.
withYDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withYDomain g t = toGDrawables (\gd -> gd{yDomain = Just t}) g

-- | Change the Z Domain of a function. This makes lists a bit complicated, see 'withNOOP'.
withZDomain :: GnuplotDrawable g => g -> (RangeComp Double, RangeComp Double) -> [GDrawable]
withZDomain g t = toGDrawables (\gd -> gd{zDomain = Just t}) g

-- | Change the style of a function. This makes lists a bit complicated, see 'withNOOP'.
withStyle :: GnuplotDrawable g => g -> Style -> [GDrawable]
withStyle g s = toGDrawables (\gd -> gd{style = s}) g

-- | A supplementary function to convert a drawable into multiple ones. Take the following example
--
-- > plot [\x -> x, sin `withXDomain` (0,1)]
--
-- The type of the second parameter would have changed and the list could not house both elements. This function is there to attach to the remaining elements, removing the type error:
--
-- > plot [withNOOP (\x -> x), sin `withXDomain` (0,1)]
withNOOP :: GnuplotDrawable g => g -> [GDrawable]
withNOOP = toGDrawables id

infixl 4 `withXDomain`, `withYDomain`, `withZDomain`, `withStyle`

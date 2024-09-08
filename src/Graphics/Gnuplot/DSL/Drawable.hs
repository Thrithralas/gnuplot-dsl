{-# LANGUAGE RecordWildCards, UndecidableInstances #-}
module Graphics.Gnuplot.DSL.Drawable (
  Style(..),
  GDrawable(..),
  GnuplotDrawable(..),
  withXDomain,
  withYDomain,
  withZDomain,
  withStyle,
  constructCommand
) where

import Graphics.Gnuplot.DSL.Expr
import Data.Default
import Data.List
import Control.Applicative
import GHC.Generics
import Data.Char

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

instance Default Style where
  def = Lines


data GDrawable = MkGDrawable {
  expr :: GExpr Double,
  xDomain :: Maybe (Double, Double),
  yDomain :: Maybe (Double, Double),
  zDomain :: Maybe (Double, Double),
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

instance GnuplotDrawable c => GnuplotDrawable [c] where
  toGDrawables f = concatMap (toGDrawables f)

instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p) where
  toGDrawables g f = [g $ def { expr = fmap (fromRational . toRational) $ f $ Var "x" }]

instance (Real a, p ~ GExpr a) => GnuplotDrawable (p -> p -> p) where
  toGDrawables g f = [g $ def { expr = fromRational . toRational <$> f (Var "x") (Var "y"), threeDimensions = True }]

withXDomain :: GnuplotDrawable g => g -> (Double, Double) -> [GDrawable]
withXDomain g t = toGDrawables (\gd -> gd { xDomain = Just t }) g

withYDomain :: GnuplotDrawable g => g -> (Double, Double) -> [GDrawable]
withYDomain g t = toGDrawables (\gd -> gd { yDomain = Just t }) g

withZDomain :: GnuplotDrawable g => g -> (Double, Double) -> [GDrawable]
withZDomain g t = toGDrawables (\gd -> gd { zDomain = Just t }) g

withStyle :: GnuplotDrawable g => g -> Style -> [GDrawable]
withStyle g s = toGDrawables (\gd -> gd { style = s }) g

infixl 4 `withXDomain`, `withYDomain`, `withZDomain`, `withStyle`

extractDomain :: [GDrawable] -> (GDrawable -> Maybe (Double, Double)) -> Maybe (Double, Double)
extractDomain [] f = Nothing
extractDomain (x : xs) f = do
  (lrest, rrest) <- extractDomain xs f <|> f x
  (l, r) <- f x <|> pure (lrest, rrest)
  pure (min l lrest, max r rrest)

constructCommand :: [GDrawable] -> String
constructCommand gds = let
  domain' = extractDomain gds xDomain
  codomain' = extractDomain gds yDomain
  thirddomain' = extractDomain gds zDomain
  hasthird = any threeDimensions gds
  in
  (if hasthird then "s" else "") ++
  "plot " ++
  maybe "[:] " (\(x,y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") domain' ++
  maybe "[:] " (\(x,y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") codomain' ++
  (if hasthird then maybe "[:] " (\(x,y) -> "[" ++ show x ++ ":" ++ show y ++ "] ") thirddomain' else "") ++
  intercalate ", " (map (\MkGDrawable { expr, style, .. } -> show expr ++ " w " ++ map toLower (show style)) gds)


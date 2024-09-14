module Graphics.Gnuplot.DSL.Process (
  plot,
  plotDebug,
  plotWithInit,
  gnuplotExec,
)
where

import Control.Applicative
import Control.Monad
import Data.List
import Graphics.Gnuplot.DSL.Command
import Graphics.Gnuplot.DSL.Drawable
import Graphics.Gnuplot.DSL.Expr
import System.Exit
import System.Process

gnuplotExec :: [GCommand] -> Bool -> IO ()
gnuplotExec cmds printoutput = do
  let commands = intercalate ";" (map show cmds)
  when printoutput $ putStrLn commands
  (e, _, r) <- readCreateProcessWithExitCode (proc "gnuplot" ["-p", "-e", commands]) ""
  putStrLn r

extractDomain :: [GDrawable] -> (GDrawable -> Maybe (RangeComp Double, RangeComp Double)) -> Maybe (RangeComp Double, RangeComp Double)
extractDomain [] f = Nothing
extractDomain (x : xs) f = do
  (lrest, rrest) <- extractDomain xs f <|> f x
  (l, r) <- f x <|> pure (lrest, rrest)
  pure (rmin l lrest, rmax r rrest)
  where
    rmax, rmin :: Ord a => RangeComp a -> RangeComp a -> RangeComp a
    rmax (RLit a) (RLit b) = RLit (max a b)
    rmax Auto x = x
    rmax x Auto = x
    rmin (RLit a) (RLit b) = RLit (min a b)
    rmin Auto x = x
    rmin x Auto = x

plotWithInit :: GnuplotDrawable g => [GCommand] -> Bool -> g -> IO ()
plotWithInit init printoutput drawable =
  let
    drawables = toGDrawables id drawable
    isThreeDim = any threeDimensions drawables
    extractor = extractDomain drawables
    plotfunc =
      if isThreeDim
        then SPlot (extractor xDomain) (extractor yDomain) (extractor zDomain) drawables
        else Plot (extractor xDomain) (extractor yDomain) drawables
   in
    gnuplotExec (init ++ [plotfunc]) printoutput

plot :: GnuplotDrawable g => g -> IO ()
plot = plotWithInit [] False

plotDebug :: GnuplotDrawable g => g -> IO ()
plotDebug = plotWithInit [] True

{-|
Module      : Graphics.Gnuplot.DSL.Process
Copyright   : (c) Márton Petes, 2024
License     : MIT
Maintainer  : tx0lwm@inf.elte.hu

Module for running the gnuplot process and sending the eDSL to it.
-}
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


-- | Execute a series of Gnuplot commands.
gnuplotExec ::
  [GCommand] -> -- ^ The series of commands to run
  Bool -> -- ^ Wether to print the generated commands passed to gnuplot
  IO ()
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

-- | Plot something with a set of inital commands.
plotWithInit ::
  GnuplotDrawable g =>
  [GCommand] -> -- ^ The series of commands to run before plotting the function(s)
  Bool -> -- ^ Wether to print the generated commands passed to gnuplot
  g -> -- ^ The thing(s) to plot
  IO ()
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

-- | Plot something. You can pass any of the following things to this function:
--
-- - A unary function:
--
-- > plot (\x -> x ** 2 + 2 * x + 1)
--
-- - A binary function:
--
-- > plot (\x y -> x * y + x / y)
--
-- - Multiple functions
--
-- > plot [\x -> sin x, \x -> cos x]
--
-- - Stylized function(s)
--
-- > plot $ (\x -> x ** 3 + 2) `withXDomain` (0, Auto)
--
-- Note that some gnuplot functions don't yet have eDSL equivalents. In the meantime, you can use the 'func' function to create arbitrary invocations
--
-- > plot (\x -> func "gamma" x)
plot :: GnuplotDrawable g => g -> IO ()
plot = plotWithInit [] False

-- | Same as 'plot', but also prints the function sent to gnuplot to stdout.
plotDebug :: GnuplotDrawable g => g -> IO ()
plotDebug = plotWithInit [] True

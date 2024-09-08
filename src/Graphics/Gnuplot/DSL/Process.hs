module Graphics.Gnuplot.DSL.Process where

import Graphics.Gnuplot.DSL.Drawable
import Graphics.Gnuplot.DSL.Expr
import System.Process
import System.Exit

plot :: GnuplotDrawable p => p -> IO ()
plot drawables = do
  let command = constructCommand (toGDrawables id drawables)
  (e, _, r) <- readCreateProcessWithExitCode (proc "gnuplot" ["-p", "-e", command]) ""
  putStrLn r

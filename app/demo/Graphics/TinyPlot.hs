{- | Graphics.TinyPlot is a minimalistic `gnuplot` wrapper -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.TinyPlot where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Monoid ((<>))
import Data.Char
import System.IO
import System.Process
import System.FilePath

data PlotData = PlotData {
    ps_filename :: String
  , ps_handle :: Handle
  } deriving(Show)

newData :: FilePath -> IO PlotData
newData ((-<.> ".dat") -> filename) = PlotData filename <$> openFile filename WriteMode

pushData :: (MonadIO m, Fractional num, Real num) => PlotData -> num -> num -> m ()
pushData PlotData{..} (fromRational . toRational -> x :: Double) (fromRational . toRational -> y :: Double) = liftIO $ do
  hPutStrLn ps_handle (show x ++ "\t" ++ show y) >> hFlush ps_handle

dat :: PlotData -> String
dat PlotData{..} =  "\"" <> ps_filename <> "\""

data Plot = Plot {
  pl_handle :: ProcessHandle
}

spawnPlot :: String -> String -> IO Plot
spawnPlot ((-<.> ".gnuplot") -> name) plot =
  Plot <$> do
    writeFile name plot *> spawnProcess "gnuplot" [name]

withPlot :: String -> String -> IO a -> IO a
withPlot ((-<.> ".gnuplot") -> name) plot h = do
  writeFile name plot
  p <- spawnProcess "gnuplot" [name]
  r <- h `finally` terminateProcess p
  return r


test :: IO ()
test = do
  putStrLn $ "Press 'd' to stop the plot"
  d <- newData "plot.dat"
  _ <- spawnPlot "plot1" $ "\n\
    \  set xrange [0:20]\n\
    \  set yrange [0:400]\n\
    \  done = 0 \n\
    \  bind all 'd' 'done = 1' \n\
    \  while(!done) {\n\
    \    plot " <> dat d <> " using 1:2 with lines\n\
    \    pause 1 \n\
    \  }\n\
    \ "

  forM_ [0..100] $ \i@(fromInteger -> r :: Double) -> do
    when (i`mod`10 == 0) $ do
      threadDelay (10^(6 :: Integer))
    pushData d r (r*r  / 3.2)



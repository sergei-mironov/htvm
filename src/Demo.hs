module Demo where

import qualified Data.Text.IO as Text

import Control.Monad.Trans
import HTVM

tputStrLn = Text.putStrLn

main :: IO ()
main = do
  return ()

demo1 :: IO Module
demo1 =
  let
    s = shape [20]
  in do
  Module <$> name "vecadd" <*> sequence [
      function "vecadd" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
        liftIO $ putStrLn "Yarr!"
        c <- compute s $ \[i] -> a![i] + b![i]
        c <- compute s $ \[i] -> a![i] + b![i]
        d <- compute [s!!0,s!!0] $ \[i,j] -> c![i,i] * c![i,i]
        e <- assign $ call "topi.relu" nullArgs [d]
        return c
    ]

demo2 :: IO Module
demo2 =
  let
    s = shape [20]
  in do
  Module <$> name "vecadd" <*> sequence [
      function "vecadd" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
        liftIO $ putStrLn "Yarr!"
        c <- compute s $ \[i] -> a![i] + b![i]
        d <- compute [s!!0,s!!0] $ \[i,j] -> c![i,i] * c![i,i]
        e <- assign $ call "topi.relu" nullArgs [d]
        return c

    , function "vecfoo" [("A",float32,s),("B",float32,s)] $ \[a,b] -> do
        c <- compute s $ \[i] -> (call "topi.relu" nullArgs [a])![i] + b![i]
        return c
    ]

putModule m = tputStrLn =<< printModule <$> m


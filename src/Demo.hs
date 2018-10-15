module Demo where

import HTVM


main :: IO ()
main = do
  return ()

demo :: IO ()
demo = do
  Module <$> pure "vecadd" <$> sequence [
      function "vecadd" [("A",float32,shape [20]), ()]
    ]
  function
  runStmtT $ do
    function

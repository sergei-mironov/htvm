{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad(when)
import Control.Concurrent(forkIO)
import Data.Monoid((<>))
import Data.IDX
import Data.Vector.Unboxed(Vector)
import System.FilePath((</>))
import System.Directory(createDirectoryIfMissing,getTemporaryDirectory,doesFileExist)
import System.Process(waitForProcess,rawSystem,runInteractiveProcess,readProcessWithExitCode)
import System.Exit(ExitCode(..))
import System.IO(stdin,stdout,stderr,hPutStrLn,hGetContents)


data MNISTOptions = MNISTOptions {
    mnist_urls :: (String,String,String,String)
  -- ^ Train images, train labels, test images test labels
  , mnist_folder :: Maybe FilePath
  -- ^ Local folder to save files to. Passing Nothing will result in using
  -- temporary folder with implementation-defined name.
  } deriving(Show,Read,Eq,Ord)


mnistFiles = (
   "train-images-idx3-ubyte"
 , "train-labels-idx1-ubyte"
 , "t10k-images-idx3-ubyte"
 , "t10k-labels-idx1-ubyte"
 )


defaultMNISTOptions :: MNISTOptions
defaultMNISTOptions = MNISTOptions {
      mnist_urls = (
          "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"
        , "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"
        , "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"
        , "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"
        )
    , mnist_folder = Nothing
  }

type URL = String

downloadFile :: URL -> FilePath -> IO ()
downloadFile url fp = do
  hPutStrLn stderr $ "Downloading '" <> url <> "' to '" <> fp <> "'"
  (pstdin,pstdout,pstderr,ph) <- runInteractiveProcess "curl" ["-o", fp, url] Nothing Nothing
  forkIO $ hPutStrLn stdout =<< hGetContents pstdout
  forkIO $ hPutStrLn stderr =<< hGetContents pstderr
  ec <- waitForProcess ph
  when (ec /= ExitSuccess) $ do
    fail $ "Failed to download '" <> url <> "' to '" <> fp

gunzipFile :: FilePath -> IO ()
gunzipFile fp = do
  ec <- rawSystem "gunzip" ["-f",fp]
  when (ec /= ExitSuccess) $ do
    fail $ "Failed to unzip '" <> fp

ensureMNISTFolder :: MNISTOptions -> IO FilePath
ensureMNISTFolder MNISTOptions{..} = do
  d <- do
    case mnist_folder of
      Just f -> pure f
      Nothing -> do
        tmp <- getTemporaryDirectory
        return $ tmp </> "mnist"
  createDirectoryIfMissing False d
  return d

ensureFile :: URL -> FilePath -> IO ()
ensureFile url fp = do
  -- doesFileExist fp >>= \case
  --   True -> return ()
  --   False -> do
      downloadFile url (fp<>".gz")
      gunzipFile (fp<>".gz")

mnistLoad :: MNISTOptions -> IO [(Int, Vector Double)]
mnistLoad o@MNISTOptions{..} = do
  f <- ensureMNISTFolder o
  let (uti,utl,uvi,uvl) = mnist_urls
  let (fti,ftl,fvi,fvl) = mnistFiles

  ensureFile uti (f</>fti)
  ensureFile utl (f</>ftl)
  ensureFile uvi (f</>fvi)
  ensureFile uvl (f</>fvl)

  Just d <- decodeIDXFile (f</>fti)
  Just l <- decodeIDXLabelsFile (f</>ftl)
  Just v <- pure $ labeledDoubleData l d
  return v


mnistTestLoad :: IO [(Int, Vector Double)]
mnistTestLoad = do
  Just d <- decodeIDXFile "_data/train-images-idx3-ubyte"
  Just l <- decodeIDXLabelsFile "_data/train-labels-idx1-ubyte"
  Just v <- pure $ labeledDoubleData l d
  return v

main :: IO ()
main = do
  return ()

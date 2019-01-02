{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MNIST (
    MNISTError(..)
  , MNISTOptions(..)
  , defaultMNISTOptions
  , downloadFile
  , mnistLoadTrainTest
  , mnistTestLoad
  ) where

import Control.Monad(join,when,liftM2)
import Control.Monad.Except(throwError,ExceptT,runExceptT)
import Control.Monad.Trans(liftIO)
import Control.Concurrent(forkIO)
import Data.Monoid((<>))
import Data.IDX
import Data.Vector.Unboxed(Vector)
import System.FilePath((</>))
import System.Directory(createDirectoryIfMissing,getTemporaryDirectory,doesFileExist)
import System.Process(waitForProcess,rawSystem,runInteractiveProcess,readProcessWithExitCode)
import System.Exit(ExitCode(..))
import System.IO(stdin,stdout,stderr,hPutStrLn,hGetContents)

data MNISTError =
    DownloadError String
  | UnpackError String
  | DecodeError
  deriving(Read,Show,Eq,Ord)

data MNISTOptions = MNISTOptions {
    mnist_urls :: (String,String,String,String)
  -- ^ Train images, train labels, test images test labels
  , mnist_folder :: Maybe FilePath
  -- ^ Local folder to save files to. Passing Nothing will result in using
  -- temporary folder with implementation-defined name.
  } deriving(Show,Read,Eq,Ord)


mnistFiles :: (FilePath,FilePath,FilePath,FilePath)
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

downloadFile :: URL -> FilePath -> ExceptT MNISTError IO ()
downloadFile url fp = do
  ec <- liftIO $ do
    hPutStrLn stderr $ "Downloading '" <> url <> "' to '" <> fp <> "'"
    (_,pstdout,pstderr,ph) <- runInteractiveProcess "curl" ["-o", fp, url] Nothing Nothing
    _ <- forkIO $ hPutStrLn stdout =<< hGetContents pstdout
    _ <- forkIO $ hPutStrLn stderr =<< hGetContents pstderr
    waitForProcess ph
  when (ec /= ExitSuccess) $ do
    throwError $ DownloadError $ "Failed to download '" <> url <> "' to '" <> fp

gunzipFile :: FilePath -> ExceptT MNISTError IO ()
gunzipFile fp = do
  ec <- liftIO $ rawSystem "gunzip" ["-f",fp]
  when (ec /= ExitSuccess) $ do
    throwError $ UnpackError $ "Failed to unzip '" <> fp

ensureMNISTFolder :: MNISTOptions -> ExceptT MNISTError IO FilePath
ensureMNISTFolder MNISTOptions{..} = liftIO $ do
  d <- do
    case mnist_folder of
      Just f -> pure f
      Nothing -> do
        tmp <- getTemporaryDirectory
        return $ tmp </> "mnist"
  createDirectoryIfMissing False d
  return d

ensureFile :: URL -> FilePath -> ExceptT MNISTError IO ()
ensureFile url fp = do
  liftIO (doesFileExist fp) >>= \case
    True -> return ()
    False -> do
      downloadFile url (fp<>".gz")
      gunzipFile (fp<>".gz")

mnistLoadTrainTest :: MNISTOptions -> IO (Either MNISTError ([(Int, Vector Double)],[(Int, Vector Double)]))
mnistLoadTrainTest o@MNISTOptions{..} = runExceptT $ do
  f <- ensureMNISTFolder o
  let (uti,utl,uvi,uvl) = mnist_urls
  let (fti,ftl,fvi,fvl) = mnistFiles

  ensureFile uti (f</>fti)
  ensureFile utl (f</>ftl)
  ensureFile uvi (f</>fvi)
  ensureFile uvl (f</>fvl)

  let decode l i = liftIO $ pure . join =<< liftM2 labeledDoubleData <$> decodeIDXLabelsFile l <*> decodeIDXFile i
  liftM2 (,) <$> decode (f</>ftl) (f</>fti) <*> decode (f</>fvl) (f</>fvi)
  >>= \case
    Just x -> return x
    Nothing -> throwError $ DecodeError

mnistTestLoad :: IO [(Int, Vector Double)]
mnistTestLoad = do
  Just d <- decodeIDXFile "_data/train-images-idx3-ubyte"
  Just l <- decodeIDXLabelsFile "_data/train-labels-idx1-ubyte"
  Just v <- pure $ labeledDoubleData l d
  return v


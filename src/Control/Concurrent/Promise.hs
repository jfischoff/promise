{-# LANGUAGE DeriveFunctor #-}

module Control.Concurrent.Promise 
  ( Promise
  , runPromise
  ) where

import Control.Concurrent.Async
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

newtype Promise a = Promise { unPromise :: IO (Async a) }
  deriving (Functor)

instance Applicative Promise where
  pure = Promise . async . return
  Promise mf <*> Promise mx = Promise $ do  
      f <- mf
      x <- mx
      (f', x') <- waitBoth f x
      async $ return $ f' x'

instance Monad Promise where
   return = liftIO . return
   Promise m >>= f = Promise $ m >>= wait >>= unPromise . f 

instance MonadIO Promise where
  liftIO = Promise . async

runPromise :: Promise a -> IO a
runPromise = wait <=< unPromise

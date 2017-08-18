{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Transient where

import Control.Concurrent.MVar
import Control.Monad (liftM, ap)
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Map as M

import System.IO.Unsafe
import Unsafe.Coerce

data EventF = forall m b c. EventF (b -> Transient m c)

data Transient m x = Transient (m (Maybe x))

runTrans :: Transient m x -> m (Maybe x)
runTrans (Transient mx) = mx

stop :: Monad m => Transient m a
stop = Transient.empty

empty :: Monad m => Transient m a
empty = Transient $ return Nothing

setEventCont :: MonadState EventF m => (b -> Transient m c) -> m EventF
setEventCont f = do
    f' <- get
    put $ EventF f
    return f'

instance MonadState EventF m => Functor (Transient m) where
  fmap = liftM

instance MonadState EventF m => Applicative (Transient m) where
  pure  = return
  (<*>) = ap

instance MonadState EventF m => Monad (Transient m) where
  return x = Transient $ return $ Just x

  x >>= f = Transient $ do
    setEventCont f
    mk <- runTrans x
    case mk of
      Just k -> runTrans (f k)
      Nothing -> return Nothing

instance MonadTrans Transient where
    lift mx = Transient $ mx >>= return . Just

instance (MonadState EventF m, MonadIO m) => MonadIO (Transient m) where
    liftIO io = let x = liftIO io in x `seq` lift x

instance (MonadState EventF m) => MonadState EventF (Transient  m) where
--  type StateType (Transient m) = EventF
    get = Transient $ get >>= return . Just
    put x = Transient $ put x >> return (Just ())

eventHandlers :: MVar (M.Map String EventF)
eventHandlers = unsafePerformIO $ newMVar M.empty

type EvType = String
data Event = forall a. Event EvType a

waitEvent name = Transient $ do
  f <- get
  evs <- liftIO $ takeMVar eventHandlers
  liftIO . putMVar eventHandlers . M.insert name f $ evs
  return Nothing

eventLoop [] = return ()
eventLoop (Event name r : evs) = do
    liftIO . putStrLn $ "new event: " ++ name
    ths <- liftIO . readMVar $ eventHandlers
    case M.lookup name ths of
      Just (EventF f) -> runTrans'' $ (unsafeCoerce f)  r
      Nothing         -> return ()
    eventLoop evs

runTrans' :: Monad m => Transient (StateT EventF m) x -> m (Maybe x)
runTrans' tmx = evalStateT (runTrans tmx) undefined

runTrans'' :: Transient (StateT EventF IO) a -> IO ()
runTrans'' tmx = runTrans' tmx >> return ()

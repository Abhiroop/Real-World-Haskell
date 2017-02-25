{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Control.Monad(liftM)
import Web.Scotty
import qualified Web.Scotty.Trans as T
import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Monoid(mconcat)
import Control.Monad.Trans.Reader



liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

{-
newtype ScottyT e m a =
     ScottyT { runS :: State (ScottyState e m) a }
     deriving ( Functor, Applicative, Monad )

newtype ActionT e m a =
     ActionT { runAM :: ExceptT (ActionError e)
                                (ReaderT ActionEnv
                                (StateT ScottyResponse m)) a }
     deriving ( Functor, Applicative )

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO
-}
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT
      . (ExceptT . fmap Right)
      . liftReaderT
      . \m->StateT(\s -> do
                       a<-m
                       return (a, s))
      ) $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-------------------------------------------------------------------

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

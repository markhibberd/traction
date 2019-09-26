{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Traction.Control (
    Db
  , DbT (..)
  , DbError (..)
  , Tracer
  , renderDbError
  , DbPool (..)
  , DbPoolConfiguration (..)
  , defaultDbPoolConfiguration
  , MonadDb (..)
  , transaction
  , transactionT
  , runDb
  , runDbT
  , runDbWith
  , runDbWithT
  , runDbTracing
  , runDbTracingT
  , runDbTracingWith
  , runDbTracingWithT
  , newPool
  , newPoolWith
  , newRollbackPool
  , newRollbackPoolWith
  , withRollbackSingletonPool
  , withConnection
  , failWith
  , withTracing
  , trace
  , noTracing
  ) where

import           Control.Monad.Catch (Exception, MonadMask (..), MonadThrow, MonadCatch, Handler (..), handle, catches, bracket_, throwM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (MFunctor (..), squash)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask, asks, local)

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time
import           Data.Typeable (Typeable)
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Simple as Postgresql

import           System.IO (IO)

import           Traction.Prelude


newtype DbPool =
  DbPool {
      runDbPool :: forall a. (Postgresql.Connection -> EitherT DbError IO a) -> EitherT DbError IO a
    }

data TransactionContext =
    InTransaction Postgresql.Connection
  | NotInTransaction DbPool

data TractionSettings =
  TractionSettings {
      transactionContext :: TransactionContext
    , tracer :: Tracer
    }

type Tracer = Text -> IO ()

noTracing :: Tracer
noTracing = const (pure ())

type Db =
  DbT IO

newtype DbT m a =
  DbT {
      _runDb :: ReaderT TractionSettings (EitherT DbError m) a
    }  deriving (Functor, Applicative, Monad, MonadIO, MonadMask, MonadThrow, MonadCatch)

instance MFunctor DbT where
  hoist f = DbT . hoist (hoist f) . _runDb

instance MonadTrans DbT where
  lift = DbT . lift . lift

data DbError =
    DbSqlError Postgresql.Query Postgresql.SqlError
  | DbQueryError Postgresql.Query Postgresql.QueryError
  | DbFormatError Postgresql.Query Postgresql.FormatError
  | DbResultError Postgresql.Query Postgresql.ResultError
  | DbTooManyResults Postgresql.Query Int
  | DbNoResults Postgresql.Query
  | DbEncodingInvariant Postgresql.Query Text Text
    deriving (Show, Eq, Typeable)

renderDbError :: DbError -> Text
renderDbError e =
  Text.pack $ case e of
    DbSqlError q err ->
      mconcat ["SQL Error [", show err, "], for query: ", show q]
    DbQueryError q err ->
      mconcat ["Query Error [", show err, "], for query: ", show q]
    DbFormatError q err ->
      mconcat ["Format Error [", show err, "], for query: ", show q]
    DbResultError q err ->
      mconcat ["Result Error [", show err, "], for query: ", show q]
    DbTooManyResults q n ->
      mconcat ["Too many results [", show n , "], for query: ", show q]
    DbNoResults q ->
      mconcat ["Query generated no results, for query: ", show q]
    DbEncodingInvariant q field encoding ->
      mconcat ["Query could not decode results, expected to be able to decode [", Text.unpack field, "], to type, [", Text.unpack encoding, "], for query: ", show q]

class MonadIO m => MonadDb m where
  liftDb :: DbT IO a -> m a

instance MonadIO m => MonadDb (DbT m) where
  liftDb = hoist liftIO

instance MonadDb m => MonadDb (ExceptT e m) where
  liftDb = lift . liftDb

data WithTransaction =
    WithTransaction
  | WithoutTransaction

failWith :: DbError -> Db a
failWith =
  DbT . lift . left

runDb :: DbPool -> Db a -> EitherT DbError IO a
runDb pool db =
  runDbWith pool WithTransaction db

runDbT :: DbPool -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbT pool handler db =
  runDbWithT pool WithTransaction handler db

runDbWith :: DbPool -> WithTransaction -> Db a -> EitherT DbError IO a
runDbWith pool tx db =
  runDbTracingWith pool noTracing tx db

runDbWithT :: DbPool -> WithTransaction -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbWithT pool tx handler db =
  runDbTracingWithT pool noTracing tx handler db

runDbTracing :: DbPool -> Tracer -> Db a -> EitherT DbError IO a
runDbTracing pool tr db =
  runDbTracingWith pool tr WithTransaction db

runDbTracingT :: DbPool -> Tracer -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbTracingT pool tr handler db =
  runDbTracingWithT pool tr WithTransaction handler db

runDbTracingWith :: DbPool -> Tracer -> WithTransaction -> Db a -> EitherT DbError IO a
runDbTracingWith pool tr tx db =
  runReaderT (_runDb $ case tx of
    WithTransaction ->
      transaction db
    WithoutTransaction ->
      db) $ TractionSettings (NotInTransaction pool) tr

runDbTracingWithT :: DbPool -> Tracer -> WithTransaction -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbTracingWithT pool tr tx handler db =
  squash $ mapEitherT (firstEitherT handler . runDbTracingWith pool tr tx) db

transaction :: Db a -> Db a
transaction db =
  DbT $ ask >>= \cc -> lift $ case transactionContext cc of
    InTransaction _ ->
      runReaderT (_runDb db) cc
    NotInTransaction pool ->
      runDbPool pool $ \c ->
        runReaderT (_runDb db) $ TractionSettings (InTransaction c) noTracing

transactionT :: EitherT e Db a -> EitherT e Db a
transactionT =
  transactional runEitherT newEitherT

transactional :: (Monad m, Monad n) => (m a -> Db (n a)) -> (Db (n a) -> m a) -> m a -> m a
transactional sifter lifter db =
  lifter . DbT $ ask >>= \cc -> lift $ case transactionContext cc of
    InTransaction c ->
      runReaderT (_runDb $ sifter db) $ TractionSettings (InTransaction c) noTracing
    NotInTransaction pool ->
      runDbPool pool $ \c ->
        runReaderT (_runDb $ sifter db) $ TractionSettings (InTransaction c) noTracing

data DbPoolConfiguration =
  DbPoolConfiguration {
      dbPoolStripes :: Int
    , dbPoolKeepAliveSeconds :: Time.NominalDiffTime
    , dbPoolSize :: Int
    } deriving (Eq, Ord, Show)

defaultDbPoolConfiguration :: DbPoolConfiguration
defaultDbPoolConfiguration =
  DbPoolConfiguration {
      dbPoolStripes = 4
    , dbPoolKeepAliveSeconds = 20
    , dbPoolSize = 20
    }

data RollbackException =
    RollbackException DbError
    deriving (Eq, Show, Typeable)

instance Exception RollbackException

newPool :: ByteString -> IO DbPool
newPool connection =
  newPoolWith connection defaultDbPoolConfiguration (pure ())

newPoolWith :: ByteString -> DbPoolConfiguration -> Db () -> IO DbPool
newPoolWith connection configuration initializer = do
  pool <- Pool.createPool
    (Postgresql.connectPostgreSQL connection)
    Postgresql.close
    (dbPoolStripes configuration)
    (dbPoolKeepAliveSeconds configuration)
    (dbPoolSize configuration)
  pure $ DbPool $ \db ->
    newEitherT $ Pool.withResource pool $ \c ->
      handle (\(RollbackException e) -> pure $ Left e) $ Postgresql.withTransaction c $ do
        r <- runEitherT $ do
          runReaderT (_runDb initializer) $ TractionSettings (InTransaction c) noTracing
          db c
        case r of
          Left e -> do
            throwM $ RollbackException e
          Right _ ->
            pure r

newRollbackPool :: ByteString -> IO DbPool
newRollbackPool connection =
  newRollbackPoolWith connection defaultDbPoolConfiguration (pure ())

newRollbackPoolWith :: ByteString -> DbPoolConfiguration -> Db () -> IO DbPool
newRollbackPoolWith connection configuration initializer = do
  pool <- Pool.createPool
    (Postgresql.connectPostgreSQL connection)
    Postgresql.close
    (dbPoolStripes configuration)
    (dbPoolKeepAliveSeconds configuration)
    (dbPoolSize configuration)
  pure $ DbPool $ \db ->
    newEitherT $ Pool.withResource pool $ \c ->
      bracket_ (Postgresql.begin c) (Postgresql.rollback c)  $ do
        runEitherT $ do
          runReaderT (_runDb initializer) $ TractionSettings (InTransaction c) noTracing
          db c

withRollbackSingletonPool :: (MonadMask m, MonadIO m) => ByteString -> (DbPool -> m a) -> m a
withRollbackSingletonPool connection action = do
  c <- liftIO . Postgresql.connectPostgreSQL $ connection
  bracket_ (liftIO $ Postgresql.begin c) (liftIO $ Postgresql.rollback c) $
    action $ DbPool $ \db -> db c

withConnection :: Postgresql.Query -> (Postgresql.Connection -> IO a) -> Db a
withConnection query f =
  DbT $ ask >>= \cc -> case transactionContext cc of
    InTransaction c ->
      lift . safely query . f $ c
    NotInTransaction pool ->
      lift . runDbPool pool $ \c -> (safely query . f $ c)

withTracing :: Tracer -> DbT m () -> DbT m ()
withTracing f (DbT db) = DbT $ local (\x -> x { tracer = f }) db

trace :: (MonadDb m, Show a) => a -> m ()
trace a =
  liftDb . DbT $ asks tracer >>= \t ->
    liftIO . t . Text.pack $ show a

safely :: Postgresql.Query -> IO a -> EitherT DbError IO a
safely query action =
  newEitherT $ catches (Right <$> action) [
      Handler $ pure . Left . DbSqlError query
    , Handler $ pure . Left . DbQueryError query
    , Handler $ pure . Left . DbFormatError query
    , Handler $ pure . Left . DbResultError query
    ]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Traction.Control (
    Db (..)
  , DbError (..)
  , renderDbError
  , DbPool (..)
  , DbPoolConfiguration (..)
  , defaultDbPoolConfiguration
  , MonadDb (..)
  , runDb
  , runDbT
  , newPool
  , newPoolWith
  , newRollbackPool
  , newRollbackPoolWith
  , withRollbackSingletonPool
  , withConnection
  , failWith
  ) where

import           Control.Monad.Catch (Exception, MonadMask (..), Handler (..), handle, catches, bracket_, throwM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (squash)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)

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
      runDbPool :: forall a. Db a -> EitherT DbError IO a
    }

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

newtype Db a =
  Db {
      _runDb :: ReaderT Postgresql.Connection (EitherT DbError IO) a
    }  deriving (Functor, Applicative, Monad, MonadIO)

class MonadIO m => MonadDb m where
  liftDb :: Db a -> m a

instance MonadDb Db where
  liftDb = id

instance MonadDb m => MonadDb (ExceptT e m) where
  liftDb = lift . liftDb

failWith :: DbError -> Db a
failWith =
  Db . lift . left

runDb :: DbPool -> Db a -> EitherT DbError IO a
runDb pool db =
  runDbPool pool db

runDbT :: DbPool -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbT pool handler db =
  squash $ mapEitherT (firstEitherT handler . runDb pool) db

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
        r <- runEitherT $ flip runReaderT c $ _runDb (initializer >> db)
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
        runEitherT $ flip runReaderT c $ _runDb (initializer >> db)

withRollbackSingletonPool :: (MonadMask m, MonadIO m) => ByteString -> (DbPool -> m a) -> m a
withRollbackSingletonPool connection action = do
  c <- liftIO . Postgresql.connectPostgreSQL $ connection
  bracket_ (liftIO $ Postgresql.begin c) (liftIO $ Postgresql.rollback c) $
    action $ DbPool $ \db ->
      newEitherT $
        runEitherT $ flip runReaderT c $ _runDb db

withConnection :: Postgresql.Query -> (Postgresql.Connection -> IO a) -> Db a
withConnection query f =
  Db $ ask >>= lift . safely query . f

safely :: Postgresql.Query -> IO a -> EitherT DbError IO a
safely query action =
  newEitherT $ catches (Right <$> action) [
      Handler $ pure . Left . DbSqlError query
    , Handler $ pure . Left . DbQueryError query
    , Handler $ pure . Left . DbFormatError query
    , Handler $ pure . Left . DbResultError query
    ]

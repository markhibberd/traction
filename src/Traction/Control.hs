{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Traction.Control (
    Db (..)
  , DbError (..)
  , renderDbError
  , DbPool
  , MonadDb (..)
  , runDb
  , runDbT
  , testDb
  , newPool
  , withConnection
  , failWith
  ) where

import           Control.Monad.Catch (Handler (..), catches, bracket)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (squash)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Reader (ReaderT (..), ask)

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Pool (Pool)
import qualified Data.Pool as Pool

import qualified Database.PostgreSQL.Simple as Postgresql

import           System.IO (IO)

import           Traction.Prelude


type DbPool =
  Pool Postgresql.Connection

data DbError =
    DbSqlError Postgresql.Query Postgresql.SqlError
  | DbQueryError Postgresql.Query Postgresql.QueryError
  | DbFormatError Postgresql.Query Postgresql.FormatError
  | DbResultError Postgresql.Query Postgresql.ResultError
  | DbTooManyResults Postgresql.Query Int
  | DbNoResults Postgresql.Query
    deriving (Show)

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

runDb :: Pool Postgresql.Connection -> Db a -> EitherT DbError IO a
runDb pool db =
  newEitherT $ Pool.withResource pool $ \c ->
    -- NOTE: this could be a lot better, withTransaction will try to commit
    --       even thought we have rolled back, ideally we would handle it in
    --       bracket code, but requires pulling in a lot of retry logic. At
    --       somepoint that is probably not the worst idea anyway.
    Postgresql.withTransaction c $ do
      r <- runEitherT $ flip runReaderT c $ _runDb db
      case r of
        Left _ -> do
          Postgresql.rollback c
          pure r
        Right _ ->
          pure r

runDbT :: Pool Postgresql.Connection -> (DbError -> e) -> EitherT e Db a -> EitherT e IO a
runDbT pool handler db =
  squash $ mapEitherT (firstEitherT handler . runDb pool) db

testDb :: Pool Postgresql.Connection -> Db a -> EitherT DbError IO a
testDb pool db =
  newEitherT $ Pool.withResource pool $ \c ->
    bracket (Postgresql.begin c >> pure c) Postgresql.rollback . const $
      runEitherT $
        flip runReaderT c $ _runDb db

newPool :: ByteString -> IO (Pool Postgresql.Connection)
newPool connection =
  Pool.createPool
    (Postgresql.connectPostgreSQL connection)
    Postgresql.close
    1
    20
    20

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

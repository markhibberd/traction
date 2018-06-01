{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Traction.Sql (
    Only (..)
  , sql
  , mandatory
  , mandatory_
  , unique
  , unique_
  , query
  , query_
  , execute
  , execute_
  , value
  , valueWith
  , values
  , valuesWith
  , Schema (..)
  , newSchema
  , Savepoint (..)
  , newSavepoint
  , createSavepoint
  , releaseSavepoint
  , rollbackSavepoint
  , Unique (..)
  , isUnique
  , isDuplicate
  , withUniqueCheck
  , withUniqueCheckSavepoint
  ) where

import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Control.Monad.IO.Class (MonadIO (..))

import qualified Data.ByteString.Builder as ByteStringBuilder
import qualified Data.Char as Char
import           Data.Data (Data)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable (Typeable)

import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple (ToRow, FromRow, Only (..))
import qualified Database.PostgreSQL.Simple as Postgresql
import qualified Database.PostgreSQL.Simple.ToField as Postgresql

import           Traction.Control
import           Traction.Prelude


mandatory :: (MonadDb m, ToRow a, FromRow b) => Postgresql.Query -> a -> m b
mandatory q =
  liftDb . definitely q . unique q

mandatory_ :: (MonadDb m, FromRow a) => Postgresql.Query -> m a
mandatory_ q =
  liftDb . definitely q $ unique_ q

unique :: (MonadDb m, ToRow a, FromRow b) => Postgresql.Query -> a -> m (Maybe b)
unique q parameters =
  liftDb . possibly q . withConnection q $ \c ->
    Postgresql.query c q parameters

unique_ :: (MonadDb m, FromRow a) => Postgresql.Query -> m (Maybe a)
unique_ q =
  liftDb . possibly q . withConnection q $ \c ->
    Postgresql.query_ c q

query :: (MonadDb m, ToRow a, FromRow  b) => Postgresql.Query -> a -> m [b]
query q parameters =
  liftDb . withConnection q $ \c ->
    Postgresql.query c q parameters

query_ :: (MonadDb m, FromRow a) => Postgresql.Query -> m [a]
query_ q =
  liftDb . withConnection q $ \c ->
    Postgresql.query_ c q

execute :: (MonadDb m, ToRow a) => Postgresql.Query -> a -> m Int64
execute q parameters =
  liftDb . withConnection q $ \c ->
    Postgresql.execute c q parameters

execute_ :: MonadDb m => Postgresql.Query -> m Int64
execute_ q =
  liftDb . withConnection q $ \c ->
    Postgresql.execute_  c q

possibly :: Postgresql.Query -> Db [a] -> Db (Maybe a)
possibly q db =
  db >>= \as ->
    case as of
      [] ->
        pure Nothing
      [x] ->
        pure . Just $ x
      (_x:_xs) ->
        failWith $ DbTooManyResults q (length as)

definitely :: Postgresql.Query -> Db (Maybe a) -> Db a
definitely q db =
  db >>=
    fromMaybeM (failWith $ DbNoResults q)

value :: Functor f => f (Only a) -> f a
value =
  fmap fromOnly

valueWith :: Functor f => (a -> b) -> f (Only a) -> f b
valueWith f =
  fmap (f . fromOnly)

values :: (Functor f, Functor g) => g (f (Only a)) -> g (f a)
values =
  (fmap . fmap) fromOnly

valuesWith :: (Functor f, Functor g) => (a -> b) -> g (f (Only a)) -> g (f b)
valuesWith f =
  (fmap . fmap) (f . fromOnly)

newtype Schema =
  Schema {
      renderSchema :: Text
    } deriving (Eq, Show, Data, Typeable)

instance Postgresql.ToField Schema where
  toField = Postgresql.EscapeIdentifier . Text.encodeUtf8 . renderSchema

newSchema :: Text -> Maybe Schema
newSchema t =
  if Text.all (\c -> Char.isAlpha c || c == '_') t
    then Just (Schema t)
    else Nothing

newtype Savepoint =
  Savepoint {
      renderSavepoint :: Text
    } deriving (Eq, Show, Data, Typeable)

instance Postgresql.ToField Savepoint where
  toField = Postgresql.Plain . ByteStringBuilder.byteString . Text.encodeUtf8 . renderSavepoint

newSavepoint :: Text -> Maybe Savepoint
newSavepoint t =
  if Text.all (\c -> Char.isAlpha c || c == '_') t
    then Just (Savepoint t)
    else Nothing

createSavepoint :: Savepoint -> Db ()
createSavepoint s =
  void $ execute [sql| SAVEPOINT ? |] (Only s)

releaseSavepoint :: Savepoint -> Db ()
releaseSavepoint s =
  void $ execute [sql| RELEASE SAVEPOINT ? |] (Only s)

rollbackSavepoint :: Savepoint -> Db ()
rollbackSavepoint s =
  void $ execute [sql| ROLLBACK TO SAVEPOINT ? |] (Only s)

bracketSavepoint :: Savepoint -> Db a -> Db a
bracketSavepoint savepoint db =
  Db $ ask >>= \c -> lift $ do
    r <- liftIO . runEitherT $ flip runReaderT c $ _runDb (createSavepoint savepoint >> db)
    case r of
      Left _ -> do
        flip runReaderT c $ _runDb (rollbackSavepoint savepoint)
        newEitherT $ pure r
      Right _ -> do
        flip runReaderT c $ _runDb (releaseSavepoint savepoint)
        newEitherT $ pure r

data Unique a =
    Unique a
  | Duplicate Postgresql.Query Postgresql.SqlError
    deriving (Show, Functor)

isUnique :: Unique a -> Bool
isUnique u =
  case u of
    Unique _ ->
      True
    Duplicate _ _ ->
      False

isDuplicate :: Unique a -> Bool
isDuplicate =
  not . isUnique

withUniqueCheck :: Db a -> Db (Unique a)
withUniqueCheck =
  withUniqueCheckSavepoint (Savepoint "duplicate_key_savepoint")

withUniqueCheckSavepoint :: Savepoint -> Db a -> Db (Unique a)
withUniqueCheckSavepoint savepoint db =
  Db $ ask >>= \c -> lift $ do
    r <- liftIO . runEitherT $ flip runReaderT c $ _runDb (bracketSavepoint savepoint db)
    case r of
      Left (DbSqlError q e) -> do
        if Postgresql.sqlState e == "23505" then
          pure (Duplicate q e)
        else
          fmap Unique $ newEitherT $ pure r
      _ ->
        fmap Unique $ newEitherT $ pure r

{-# LANGUAGE NoImplicitPrelude #-}
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
  , values
  ) where

import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple (ToRow, FromRow, Only (..))
import qualified Database.PostgreSQL.Simple as Postgresql

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

execute :: (MonadDb m, ToRow a) => Postgresql.Query -> a -> m ()
execute q parameters =
  liftDb . void . withConnection q $ \c ->
    Postgresql.execute c q parameters

execute_ :: MonadDb m => Postgresql.Query -> m ()
execute_ q =
  liftDb . void . withConnection q $ \c ->
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

values :: (Functor f, Functor g) => g (f (Only a)) -> g (f a)
values =
  (fmap . fmap) fromOnly

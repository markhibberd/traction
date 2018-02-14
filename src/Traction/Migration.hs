{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Traction.Migration (
    Migration (..)
  , migrate
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Set as Set

import           Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Simple as Postgresql

import           Traction.Prelude
import           Traction.Control
import qualified Traction.Sql as Sql


data Migration =
   Migration {
       migrationName :: Text
     , migrationQuery :: Postgresql.Query
     } deriving (Eq, Show)

migrate :: [Migration] -> Db [Migration]
migrate migrations = do
  void $ Sql.execute_ "SET client_min_messages TO WARNING"
  void $ Sql.execute_ [sql| CREATE TABLE IF NOT EXISTS migrations (migration TEXT PRIMARY KEY) |]
  installed <- (fmap . fmap) Postgresql.fromOnly $ Sql.query_ [sql| SELECT migration FROM migrations |]
  forM (diff migrations installed) $ \migration -> do
    void $ Sql.execute_ $ migrationQuery migration
    void $ Sql.execute [sql| INSERT INTO migrations (migration) VALUES (?) |] (Postgresql.Only $ migrationName migration)
    pure migration

diff :: [Migration] -> [Text] -> [Migration]
diff migrations installed =
  let
    installed' = Set.fromList installed
    missing = List.filter (\x -> not . Set.member (migrationName x) $ installed') migrations
  in
    missing

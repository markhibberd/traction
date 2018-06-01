{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Traction where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Morph (hoist, lift)

import           Data.Text (Text)

import           Traction.Prelude
import           Traction.Control
import           Traction.Migration
import           Traction.Sql

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)

prop_insert_unique :: Property
prop_insert_unique =
  property $ do
    organisation <- forAll genOrganisation
    r <- db $ do
      o1 <- withUniqueCheck $ oinsert organisation
      o2 <- withUniqueCheck $ oinsert organisation
      pure (isUnique o1, isDuplicate o2)
    (True, True) === r

prop_insert_exists :: Property
prop_insert_exists =
  property $ do
    organisation <- forAll genOrganisation
    r <- db $ do
      void $ oinsert organisation
      oexists organisation
    assert r

prop_insert_not_exists :: Property
prop_insert_not_exists =
  property $ do
    organisation <- forAll genOrganisation
    r <- db $ do
      oexists organisation
    assert (not r)

prop_rollback :: Property
prop_rollback =
  property $ do
    pool <- liftIO $ mkPool
    x <- liftIO . runEitherT . runDb pool $ do
      -- NOTE: intentional syntax error
      fmap (== Just True) . values $ unique [sql|
          SELECT_FAUX_PAUX TRUE
            FROM organisation o
           WHERE o.name = ?
        |] (Only True)
    case x of
      Left _ ->
        pure ()
      Right _ ->
        failure

prop_schema :: Property
prop_schema =
  property $ do
    pool <- liftIO $ mkPool
    evalExceptT . hoist lift . runDb pool $ do
      void $ migrate schema
      void $ migrate schema

oexists :: MonadDb m => Text -> m Bool
oexists o =
  fmap (== Just True) . values $ unique [sql|
      SELECT TRUE
        FROM organisation o
       WHERE o.name = ?
    |] (Only o)

oinsert :: MonadDb m => Text -> m Int
oinsert o =
  value $ mandatory [sql|
      INSERT INTO organisation (name)
           VALUES (?)
        RETURNING id
    |] (Only o)

schema :: [Migration]
schema = [
    Migration "create-organisation" [sql|
      CREATE TABLE organisation (
          id serial PRIMARY KEY
        , name text NOT NULL UNIQUE
        )
    |]
  , Migration "create-account" [sql|
      CREATE TABLE account (
          id SERIAL PRIMARY KEY
        , organisation BIGINT NOT NULL REFERENCES organisation(id)
        , email TEXT NOT NULL UNIQUE
        , name TEXT NOT NULL
        , crypted TEXT NOT NULL
        )
    |]
  ]

db :: Db a -> PropertyT IO a
db x = do
  pool <- liftIO mkPool
  evalExceptT . hoist lift . runDb pool $ migrate schema >> x

mkPool :: IO DbPool
mkPool =
  newRollbackPool "dbname=traction_test host=localhost user=traction_test password=traction_test port=5432"

checkDb :: MonadIO m => Group -> m Bool
checkDb group =
  case group of
    Group name properties ->
      checkSequential (Group name ((fmap . fmap) (withTests 5) properties))

genOrganisation :: Gen Text
genOrganisation = do
  cooking <- Gen.element [
      "fried"
    , "diced"
    , "stewed"
    , "broiled"
    ]
  muppet <- Gen.element [
      "kermet"
    , "gonzo"
    , "beaker"
    , "statler"
    , "waldorf"
    ]
  pure $ cooking <> "-" <> muppet

tests :: IO Bool
tests =
  checkDb $$(discover)

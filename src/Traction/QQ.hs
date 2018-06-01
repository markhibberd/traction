{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Traction.QQ (
    savepoint
  , schema
  , sql
  ) where

import qualified Data.Text as Text

import           Database.PostgreSQL.Simple.SqlQQ (sql)

import qualified Prelude

import           Traction.Sql (newSchema, newSavepoint)

schema :: QuasiQuoter
schema =
  QuasiQuoter {
    quoteExp = \s -> case newSchema (Text.pack s) of
      Nothing ->
        Prelude.error "Failed to parse savepoint"
      Just v ->
        dataExp v
  , quotePat = Prelude.error "not able to qq pats"
  , quoteType = Prelude.error "not able to qq types"
  , quoteDec = Prelude.error "not able to qq decs"
  }

savepoint :: QuasiQuoter
savepoint =
  QuasiQuoter {
    quoteExp = \s -> case newSavepoint (Text.pack s) of
      Nothing ->
        Prelude.error "Failed to parse savepoint"
      Just v ->
        dataExp v
  , quotePat = Prelude.error "not able to qq pats"
  , quoteType = Prelude.error "not able to qq types"
  , quoteDec = Prelude.error "not able to qq decs"
  }

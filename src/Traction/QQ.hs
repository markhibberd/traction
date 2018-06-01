{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Traction.QQ (
    savepoint
  , schema
  , sql
  ) where

import           Data.Data (Data)
import           Data.Generics (extQ)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.PostgreSQL.Simple.SqlQQ (sql)

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import qualified Prelude

import           Traction.Sql (newSchema, newSavepoint)
import           Traction.Prelude

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

dataExp :: Data a => a -> Q Exp
dataExp a =
  dataToExpQ (const Nothing `extQ` textExp) a

textExp :: Text -> Maybe ExpQ
textExp =
  pure . appE (varE 'Text.pack) . litE . StringL . Text.unpack

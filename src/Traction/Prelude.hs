{-# LANGUAGE NoImplicitPrelude #-}
module Traction.Prelude (
    module X
  , fromMaybeM
  , whenM
  , unlessM
  , with
  ) where

import           Control.Applicative as X
import           Control.Monad as X
import           Data.Bool as X (Bool (..), (||), (&&), not, bool, otherwise)
import           Data.Char as X (Char)
import           Data.Bifunctor as X (Bifunctor (..))
import           Data.Either as X (Either (..), either)
import           Data.Foldable as X
import           Data.Function as X ((.), ($), (&), flip, id, const)
import           Data.Functor as X (($>))
import           Data.Int as X
import           Data.Maybe as X (Maybe (..), maybe, fromMaybe)
import           Data.Monoid as X (Monoid (..), (<>))
import           Data.Traversable as X
import           Prelude as X (Eq (..), Show (..), Ord (..), Num (..), Enum, Bounded (..), Integral (..), Double, error, seq, fromIntegral, (/), (^), fst, snd)
import           Text.Read as X (Read (..), readMaybe)
import           Traction.EitherT as X


fromMaybeM :: Applicative f => f a -> Maybe a -> f a
fromMaybeM =
  flip maybe pure

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

with :: Functor f => f a -> (a -> b) -> f b
with =
  flip fmap

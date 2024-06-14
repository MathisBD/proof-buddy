-- This modules defines the type of free variable identifiers and operations on it.
-- Internally free variables identifiers are represented as an integer,
-- but you should really think of them as an abstract identifier.

module PB.Data.FVarId (FVarId, MonadGenFVarId) where

-- | A free variable identifer.
newtype FVarId = FVarId Int
  deriving (Show, Eq, Ord)

-- | Typeclass for a monad that supports generating (fresh) free
-- | variable identifiers.
class (Monad m) => MonadGenFVarId m where
  genFVarId :: m FVarId

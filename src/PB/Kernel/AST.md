{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Kernel.AST (Name (..)) where

import Data.Bitfield (Bitfield)
import Data.Bitfield qualified as Bitfield
import Data.Function (on)
import Data.Int
import Data.Text (Text)
import Data.Word
import GHC.Generics (Generic)

-- | A name. Used for constants and binders.
newtype Name = Name {unName :: Text} deriving (Show, Eq, Ord)

-- | A free variable identifier.
newtype FVarId = FVarId Int deriving (Show, Eq, Ord)

-- | A metavariable identifier.
newtype MVarId = MVarId Int deriving (Show, Eq, Ord)

-- | A lambda or product binder.
data Binder = Anonymous | Named Name
  deriving (Show, Eq, Ord)

-- | Currently we have [Type :: Type] : we will switch to a universe hierarchy later on.
data Level = Type | Prop
  deriving (Show, Eq, Ord)


-- | Some cached properties associated to a term.
data Props = CData {hasMVars :: Bool, hasFVars :: Bool, looseBVarRange :: Int16}
  deriving (Show, Eq, Ord, Generic)

-- | For efficiency reasons we pack cached properties into a machine word.
type PackedProps = Bitfield Word32 Props

-- | Comparison on PackedProps is done by comparing the underlying machine words.
instance Ord PackedProps where
  compare :: PackedProps -> PackedProps -> Ordering
  compare = compare `on` Bitfield.unwrap

mergeCData :: PackedCData -> PackedCData -> PackedCData
mergeCData x y =
  Bitfield.pack $
    CData
      { hasMVars = x.hasMVars || y.hasMVars,
        hasFVars = x.hasFVars || y.hasFVars,
        looseBVarRange = max x.looseBVarRange y.looseBVarRange
      }

-- Lift some packed data through a
liftCData :: PackedCData -> PackedCData
liftCData x =
  Bitfield.pack $
    CData
      { hasMVars = x.hasMVars,
        hasFVars = x.hasFVars,
        looseBVarRange = 1 + x.looseBVarRange
      }

---------------------------------------------------------------------------------------------------
-- Terms
---------------------------------------------------------------------------------------------------

data Term
  = -- | Bound variable. The integer is a de Buijn index, starting at 0.
    BVar Int
  | -- | Free variable.
    FVar FVarId
  | -- | Meta variable.
    MVar MVarId
  | -- | Sort.
    Sort Level
  | -- | Global constant.
    Cst Name
  | -- | Lambda abstraction.
    Lambda PackedCData Binder Term Term
  | -- | Dependent product.
    Prod PackedCData Binder Term Term
  | -- | Function application.
    -- We maintain the invariant that the function is not itself an application,
    -- and that the argument list is not empty.
    App PackedCData Term [Term]
  deriving (Show, Eq, Ord)

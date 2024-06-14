{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

module PB.Data.Term
  ( Level (..),
    Binder (..),
    Term (BVar, FVar, Sort, Cst, Lambda, Prod, App),
    hasFVars,
    hasLooseBVars,
    looseBVarRange,
  )
where

import Data.Bitfield (Bitfield)
import Data.Bitfield qualified as Bitfield
import Data.Function (on)
import Data.Int
import Data.List qualified as List
import Data.Word
import GHC.Generics (Generic)
import PB.Data.FVarId
import PB.Data.Name

---------------------------------------------------------------------------------------------------
-- Term properties (implementation detail)
---------------------------------------------------------------------------------------------------

-- | Properties associated to a term.
data Props = Props
  { -- | Does this term contain free variables (FVar) ?
    hasFVars_ :: Bool,
    -- | The loose bound variables (BVar) in this term are in the range [0..looseBVarRange-1].
    -- | In particular, looseBVarRange=0 means that the term has no loose BVar.
    looseBVarRange_ :: Int16
  }
  deriving (Show, Eq, Ord, Generic)

-- | For efficiency reasons we pack properties inside an machine word.
newtype PackedProps = PackedProps {unPackedProps :: Bitfield Word32 Props}
  deriving (Show, Eq)

-- | Comparing packed properties is done by comparing the underlying machine word.
instance Ord PackedProps where
  compare :: PackedProps -> PackedProps -> Ordering
  compare = compare `on` Bitfield.unwrap . unPackedProps

-- | The packed properties for an 'empty' term.
emptyPackedProps :: PackedProps
emptyPackedProps = PackedProps $ Bitfield.pack $ Props {hasFVars_ = False, looseBVarRange_ = 0}

-- | Merge the packed properties of two terms.
-- | We assume that the terms live at the same level.
-- | This is what is used e.g. when calculating the properties of an [App].
mergePackedProps :: PackedProps -> PackedProps -> PackedProps
mergePackedProps (PackedProps x) (PackedProps y) =
  PackedProps $
    Bitfield.pack $
      Props
        { hasFVars_ = x.hasFVars_ || y.hasFVars_,
          looseBVarRange_ = max x.looseBVarRange_ y.looseBVarRange_
        }

-- | Lift the packed properties of a term through a binder.
-- | This is what is used e.g. when calculating the properties of a [Lambda].
liftPackedProps :: PackedProps -> PackedProps
liftPackedProps (PackedProps x) =
  PackedProps $
    Bitfield.pack $
      Props
        { hasFVars_ = x.hasFVars_,
          looseBVarRange_ = if x.looseBVarRange_ > 0 then x.looseBVarRange_ - 1 else 0
        }

---------------------------------------------------------------------------------------------------
-- Terms
---------------------------------------------------------------------------------------------------

data Level = Prop | Type
  deriving (Show, Eq, Ord)

data Binder = Anonymous | Named Name
  deriving (Show, Eq, Ord)

data Term
  = -- | Bound variable. The integer is a de Buijn index, starting at 0.
    BVar Int
  | -- | Free variable.
    FVar FVarId
  | -- | Sort.
    Sort Level
  | -- | Global constant.
    Cst Name
  | -- | Lambda abstraction.
    RawLambda PackedProps Binder Term Term
  | -- | Dependent product.
    RawProd PackedProps Binder Term Term
  | -- | Function application.
    -- | We maintain the invariant that the function is not itself an application,
    -- | and that the argument list is not empty.
    RawApp PackedProps Term [Term]
  deriving (Show, Eq, Ord)

-- | Retrieve or compute the packed properties associated to a term.
-- | This is O(1).
getPackedProps :: Term -> PackedProps
getPackedProps (BVar n) =
  PackedProps $
    Bitfield.pack $
      Props {hasFVars_ = False, looseBVarRange_ = fromIntegral n + 1}
getPackedProps (FVar _) =
  PackedProps $
    Bitfield.pack $
      Props {hasFVars_ = True, looseBVarRange_ = 0}
getPackedProps (Sort _) = emptyPackedProps
getPackedProps (Cst _) = emptyPackedProps
getPackedProps (RawLambda props _ _ _) = props
getPackedProps (RawProd props _ _ _) = props
getPackedProps (RawApp props _ _) = props

---------------------------------------------------------------------------------------------------
-- Custom patterns
---------------------------------------------------------------------------------------------------

-- We define (explicitly bidirectionnal) patterns so that the user
-- never has to worry about packed properties.

pattern Lambda :: Binder -> Term -> Term -> Term
pattern Lambda x ty body <- RawLambda _ x ty body
  where
    Lambda x ty body =
      let props = mergePackedProps (getPackedProps ty) (liftPackedProps $ getPackedProps body)
       in RawLambda props x ty body

pattern Prod :: Binder -> Term -> Term -> Term
pattern Prod x ty body <- RawProd _ x ty body
  where
    Prod x ty body =
      let props = mergePackedProps (getPackedProps ty) (liftPackedProps $ getPackedProps body)
       in RawProd props x ty body

pattern App :: Term -> [Term] -> Term
pattern App f args <- RawApp _ f args
  where
    App f args =
      let props = List.foldl' mergePackedProps (getPackedProps f) $ map getPackedProps args
       in RawApp props f args

---------------------------------------------------------------------------------------------------
-- Accessing the packed properties of terms
---------------------------------------------------------------------------------------------------

-- | Check if a term contains free variables. This is O(1).
hasFVars :: Term -> Bool
hasFVars term = props.hasFVars_
  where
    (PackedProps props) = getPackedProps term

-- | Check if a term contains loose bound variables. This is O(1).
hasLooseBVars :: Term -> Bool
hasLooseBVars term = props.looseBVarRange_ > 0
  where
    (PackedProps props) = getPackedProps term

-- | Get the range of loose bound variables in a term.
-- | Assuming this returns [n] the term has loose variables in range [0..n-1].
-- | This is O(1).
looseBVarRange :: Term -> Int
looseBVarRange term = fromIntegral props.looseBVarRange_
  where
    (PackedProps props) = getPackedProps term

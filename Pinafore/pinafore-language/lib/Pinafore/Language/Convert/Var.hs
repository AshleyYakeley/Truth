module Pinafore.Language.Convert.Var where

import Import
import Pinafore.Language.Convert.HasType

newtype Var (name :: Symbol) = MkVar
    { unVar :: UVarT name
    }

-- Var Type
instance
    forall (pshim :: PolyShimKind) polarity name.
    ( CCRVariancesShim pshim
    , JoinMeetIsoShim (pshim Type)
    , CoerceShim (pshim Type)
    , Is PolarityType polarity
    , KnownSymbol name
    ) =>
    HasQType pshim polarity (Var name)
    where
    qType =
        shimWitToDolan
            $ MkShimWit (VarDolanSingularType $ MkTypeVar $ MkSymbolType @name)
            $ case polarityType @polarity of
                PositiveType -> MkPolarShim $ coerceShim "var"
                NegativeType -> MkPolarShim $ coerceShim "var"

type A = Var "a"

type B = Var "b"

type C = Var "c"

type D = Var "d"

type E = Var "e"

type X = Var "x"

type Y = Var "y"

type P = Var "p"

type Q = Var "q"

type AP = Var "ap"

type BP = Var "bp"

type CP = Var "cp"

type AQ = Var "aq"

type BQ = Var "bq"

type CQ = Var "cq"

type AX = Var "ax"

type BX = Var "bx"

type CX = Var "cx"

type AY = Var "ay"

type BY = Var "by"

type CY = Var "cy"

{-
mkTypeVar :: forall k (name :: Symbol). KnownSymbol name => TypeVar (UVar k name)
mkTypeVar = MkTypeVar $ MkSymbolType @name

mkVar :: forall (name :: Symbol). KnownSymbol name => TypeVarT (Var name)
mkVar = MkTypeVar foo

-}

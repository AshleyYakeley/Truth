module Language.Expression.Dolan.SubtypeEntry.Knowledge where

import Shapes

import Language.Expression.Dolan.TypeSystem

type family DolanSubtypeHint (ground :: GroundTypeKind) :: Type

data SubtypeKnowledge (ground :: GroundTypeKind)
    = UnknownSK
    | NeutralSK
    | HintSK (DolanSubtypeHint ground)

instance forall (ground :: GroundTypeKind). Eq (DolanSubtypeHint ground) => Eq (SubtypeKnowledge ground) where
    NeutralSK == NeutralSK = True
    (HintSK a) == (HintSK b) = a == b
    _ == _ = False

instance forall (ground :: GroundTypeKind). Semigroup (DolanSubtypeHint ground) => Semigroup (SubtypeKnowledge ground) where
    NeutralSK <> sk = sk
    sk <> NeutralSK = sk
    HintSK a <> HintSK b = HintSK $ a <> b
    _ <> _ = UnknownSK

instance forall (ground :: GroundTypeKind). Show (DolanSubtypeHint ground) => Show (SubtypeKnowledge ground) where
    show UnknownSK = "unknown"
    show NeutralSK = "neutral"
    show (HintSK hint) = "hint: " <> show hint

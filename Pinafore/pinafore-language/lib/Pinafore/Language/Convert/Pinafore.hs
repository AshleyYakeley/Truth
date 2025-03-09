{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Pinafore where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

data LangType
    = forall a. MkLangType (QNonpolarType a)

instance HasQGroundType '[] LangType where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangType)|]) "Type.Pinafore."

instance ShowText LangType where
    showText (MkLangType t) = toText $ exprShow t

-- LangOpenType
data LangOpenType (pq :: (Type, Type))
    = forall a. MkLangOpenType
        (QRange a pq)
        (QNonpolarType a)

instance CatFunctor (CatRange (->)) (->) LangOpenType where
    cfmap f (MkLangOpenType r v) = MkLangOpenType (cfmap f r) v

instance ShowText (LangOpenType pq) where
    showText (MkLangOpenType _ v) = toText $ exprShow v

instance MaybeRepresentational LangOpenType where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangOpenType

instance HasQGroundType '[ 'RangeCCRVariance] LangOpenType where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangOpenType)|]) "OpenType.Pinafore."

openLangTypeToType :: forall p q. LangOpenType '(p, q) -> LangType
openLangTypeToType (MkLangOpenType _ t) = MkLangType t

mkLangTypeValue :: Some QNonpolarType -> QValue
mkLangTypeValue (MkSome (tw :: _ t)) = let
    stype :: QShimWit 'Positive (LangOpenType '(t, t))
    stype = rangeShimWit qGroundType (nonpolarToNegative @QTypeSystem tw) (nonpolarToPositive @QTypeSystem tw)
    sval :: LangOpenType '(t, t)
    sval = MkLangOpenType identityRange tw
    in MkSomeOf stype sval

-- QInterpreter
instance HasQGroundType '[CoCCRVariance] QInterpreter where
    qGroundType =
        stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QInterpreter)|]) "Interpreter.Pinafore."

newtype LangExpression = MkLangExpression
    { unLangExpression :: QExpression
    }

-- LangExpression
instance HasQGroundType '[] LangExpression where
    qGroundType =
        stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangExpression)|]) "Expression.Pinafore."

-- QDeclarations
instance HasQGroundType '[] QDeclarations where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QDeclarations)|]) "Declarations.Pinafore."

-- QScope
instance HasQGroundType '[] QScope where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QScope)|]) "Scope.Pinafore."

-- Anchor
instance HasQGroundType '[] Anchor where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Anchor)|]) "Anchor.Pinafore."

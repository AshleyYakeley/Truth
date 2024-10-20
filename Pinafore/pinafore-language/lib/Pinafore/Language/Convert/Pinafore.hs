{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Pinafore where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

-- LangType
data LangType (pq :: (Type, Type)) =
    forall a. MkLangType (QRange a pq)
                         (QNonpolarType a)

instance CatFunctor (CatRange (->)) (->) LangType where
    cfmap f (MkLangType r v) = MkLangType (cfmap f r) v

instance ShowText (LangType pq) where
    showText (MkLangType _ v) = toText $ exprShow v

instance MaybeRepresentational LangType where
    maybeRepresentational = Nothing

instance HasCCRVariance 'RangeCCRVariance LangType

instance HasQGroundType '[ RangeCCRVariance] LangType where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangType)|]) "Type.Pinafore."

mkLangTypeValue :: Some QNonpolarType -> QValue
mkLangTypeValue (MkSome (tw :: _ t)) = let
    stype :: QShimWit 'Positive (LangType '( t, t))
    stype = rangeShimWit qGroundType (nonpolarToNegative @QTypeSystem tw) (nonpolarToPositive @QTypeSystem tw)
    sval :: LangType '( t, t)
    sval = MkLangType identityRange tw
    in MkSomeOf stype sval

-- QInterpreter
instance HasQGroundType '[ CoCCRVariance] QInterpreter where
    qGroundType =
        stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QInterpreter)|]) "Interpreter.Pinafore."

newtype LangExpression = MkLangExpression
    { unLangExpression :: QExpression
    }

-- LangExpression
instance HasQGroundType '[] LangExpression where
    qGroundType =
        stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangExpression)|]) "Expression.Pinafore."

-- QScopeDocs
instance HasQGroundType '[] QScopeDocs where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QScopeDocs)|]) "Scope.Pinafore."

-- Anchor
instance HasQGroundType '[] Anchor where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Anchor)|]) "Anchor.Pinafore."

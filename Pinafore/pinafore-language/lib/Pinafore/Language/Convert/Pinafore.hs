{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Convert.Pinafore where

import Import
import Pinafore.Language.Convert.HasType
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

instance HasQGroundType '[] ActionException where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ActionException)|]) "Exception"

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

instance Exception QError

-- QError
instance HasQGroundType '[] QError where
    qGroundType = let
        gds :: QPolyGreatestDynamicSupertype '[] QError
        gds =
            varPolyGreatestDynamicSupertype
                NilCCRArguments
                $ mapNegShimWit
                    ( functionToShim "" $ \case
                        ExActionException se -> fromException se
                        _ -> Nothing
                    )
                    (qGroundedType :: _ ActionException)
        in (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QError)|]) "Error.Pinafore.")
            { qgtGreatestDynamicSupertype = gds
            }

-- Located
instance HasVariance Located where
    type VarianceOf Located = 'Covariance

-- QItem
instance HasQGroundType '[] QItem where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QItem)|]) "Item.Pinafore."

-- QScopeItem
instance HasQGroundType '[] QScopeItem where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QScopeItem)|]) "ScopeItem.Pinafore."

-- Anchor
instance HasQGroundType '[] Anchor where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Anchor)|]) "Anchor.Pinafore."

{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Pinafore
    ( pinaforeLibSection
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var

-- QContext
newtype QContext =
    MkQContext LibraryContext

instance HasQGroundType '[] QContext where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QContext)|]) "Context.Pinafore."

thisContext :: QSpecialForm
thisContext =
    MkQSpecialForm NilListType $ \() -> do
        lm <- paramAsk loadModuleParam
        let
            sval :: QContext
            sval = MkQContext $ MkLibraryContext lm
        return $ constSealedExpression $ MkSomeOf qType sval

-- QInterpreter
instance HasQGroundType '[ CoCCRVariance] QInterpreter where
    qGroundType =
        stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QInterpreter)|]) "Interpreter.Pinafore."

langRunInterpreter :: QContext -> QInterpreter A -> Action (Result Text A)
langRunInterpreter (MkQContext lc) ia = let
    ?library = lc
    in do
           rea <- runInterpretResult $ runPinaforeScoped "<evaluate>" ia
           return $ mapResultFailure showText rea

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

langTypePositive :: LangType '( p, q) -> QShimWit 'Positive p
langTypePositive (MkLangType r t) = mapShimWit (MkPolarShim $ rangeContra r) $ nonpolarToPositive @QTypeSystem t

langTypeNegative :: LangType '( p, q) -> QShimWit 'Negative q
langTypeNegative (MkLangType r t) = mapShimWit (MkPolarShim $ rangeCo r) $ nonpolarToNegative @QTypeSystem t

langUnifyTypes :: LangType '( A, TopType) -> LangType '( BottomType, B) -> QInterpreter (A -> B)
langUnifyTypes ta tb = fmap shimToFunction $ qUnifyRigid (langTypePositive ta) (langTypeNegative tb)

-- LangValue
newtype LangValue =
    MkLangValue QValue

instance HasQGroundType '[] LangValue where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangValue)|]) "Value.Pinafore."

mkLangValue :: LangType '( A, TopType) -> A -> LangValue
mkLangValue t v = MkLangValue $ MkSomeOf (langTypePositive t) v

interpretToValue :: Text -> QInterpreter LangValue
interpretToValue src = fmap MkLangValue $ parseToValue src []

langUnifyValue :: LangType '( BottomType, A) -> LangValue -> QInterpreter A
langUnifyValue t (MkLangValue v) = qUnifyValueTo (langTypeNegative t) v

pinaforeLibSection :: LibraryStuff
pinaforeLibSection =
    headingBDS "Pinafore" "" $
    pure $
    namespaceBDS
        "Pinafore"
        [ typeBDS "Context" "" (qSomeGroundType @_ @QContext) []
        , namespaceBDS
              "Context"
              [specialFormBDS "this" "The context at this point in source." [] "Context.Pinafore" thisContext]
        , typeBDS "Interpreter" "" (qSomeGroundType @_ @QInterpreter) []
        , namespaceBDS "Interpreter" $ monadEntries @Interpreter <> [valBDS "run" "" langRunInterpreter]
        , typeBDS "Type" "" (qSomeGroundType @_ @LangType) []
        , specialFormBDS "const.Type" "" ["@A"] "Type.Pinafore A" $
          MkQSpecialForm (ConsListType AnnotNonpolarType NilListType) $ \(MkSome (tw :: _ t), ()) -> let
              stype :: QShimWit 'Positive (LangType '( t, t))
              stype = rangeShimWit qGroundType (nonpolarToNegative @QTypeSystem tw) (nonpolarToPositive @QTypeSystem tw)
              sval :: LangType '( t, t)
              sval = MkLangType identityRange tw
              in return $ constSealedExpression $ MkSomeOf stype sval
        , hasSubtypeRelationBDS @(LangType '( P, Q)) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "Type" [valBDS "unify" "Unify two types." langUnifyTypes]
        , typeBDS "Value" "" (qSomeGroundType @_ @LangValue) []
        , namespaceBDS
              "Value"
              [ valBDS "mk" "" mkLangValue
              , valBDS "unify" "Unify type with value." langUnifyValue
              , valBDS "interpret" "Interpret Pinafore source." interpretToValue
              ]
        ]

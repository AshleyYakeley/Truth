{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Pinafore
    ( pinaforeLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

-- QContext
newtype QContext
    = MkQContext LibraryContext

instance HasQGroundType '[] QContext where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QContext)|]) "Context.Pinafore."

getContext :: QInterpreter QContext
getContext = do
    lm <- paramAsk loadModuleParam
    return $ MkQContext $ MkLibraryContext lm

thisContext :: QInterpreter LangExpression
thisContext = do
    sval <- getContext
    return $ MkLangExpression $ qConst sval

getScope :: QInterpreter QScope
getScope = paramAsk scopeParam

thisScope :: QInterpreter LangExpression
thisScope = do
    sval <- getScope
    return $ MkLangExpression $ qConst sval

langRunInterpreter :: QContext -> QInterpreter A -> Action (Result Text A)
langRunInterpreter (MkQContext lc) ia = let
    ?library = lc
    in do
        rea <- runInterpretResult $ runPinaforeScoped "<evaluate>" ia
        return $ mapResultFailure showText rea

langTypePositive :: LangOpenType '(p, q) -> QShimWit 'Positive p
langTypePositive (MkLangOpenType r t) = mapShimWit (MkPolarShim $ rangeContra r) $ nonpolarToPositive @QTypeSystem t

langTypeNegative :: LangOpenType '(p, q) -> QShimWit 'Negative q
langTypeNegative (MkLangOpenType r t) = mapShimWit (MkPolarShim $ rangeCo r) $ nonpolarToNegative @QTypeSystem t

langUnifyOpenTypes :: LangOpenType '(A, TopType) -> LangOpenType '(BottomType, B) -> QInterpreter (A -> B)
langUnifyOpenTypes ta tb = fmap shimToFunction $ qUnifyRigid (langTypePositive ta) (langTypeNegative tb)

{-
langUnifyOpenTypes :: QScope -> LangOpenType '(A, TopType) -> LangOpenType '(BottomType, B) -> QTypeResult (A -> B)
langUnifyOpenTypes (MkQContext lc (MkWRaised runTypeM)) ta tb = fmap shimToFunction $ runTypeM $ tsUnifyRigid @QTypeSystem (langTypePositive ta) (langTypeNegative tb)
-}

-- LangValue
newtype LangValue = MkLangValue
    { unLangValue :: QValue
    }

instance HasQGroundType '[] LangValue where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangValue)|]) "Value.Pinafore."

mkLangValue :: LangOpenType '(A, TopType) -> A -> LangValue
mkLangValue t v = MkLangValue $ MkSomeOf (langTypePositive t) v

interpretToExpression :: Text -> QInterpreter LangExpression
interpretToExpression src = fmap MkLangExpression $ parseTopExpression src

evaluateLangExpression :: LangExpression -> QInterpreter LangValue
evaluateLangExpression (MkLangExpression expr) = do
    val <- qEvalExpr expr
    return $ MkLangValue val

interpretToValue :: Text -> QInterpreter LangValue
interpretToValue src = do
    expr <- interpretToExpression src
    evaluateLangExpression expr

langUnifyValue :: LangOpenType '(BottomType, A) -> LangValue -> QInterpreter A
langUnifyValue t (MkLangValue v) = qUnifyValueTo (langTypeNegative t) v

langWithScope :: QDeclarations -> QInterpreter A -> QInterpreter A
langWithScope sdocs = withDeclarations sdocs

bindScope :: Text -> LangValue -> QDeclarations
bindScope name (MkLangValue val@(MkSomeFor t _)) = let
    fname = fromString $ unpack name
    doc :: DefDoc
    doc = MkDefDoc{docItem = ValueDocItem (pure $ fullNameRef fname) $ exprShow t, docDescription = ""}
    in declarations $ bindingInfosToScope $ pure $ (fname, MkQBindingInfo fname doc $ ValueBinding $ qConstValue val)

interpretModuleFromSource :: Text -> QInterpreter QDeclarations
interpretModuleFromSource src = do
    m <- parseModule src
    return $ moduleDeclarations m

pinaforeLibSection :: LibraryStuff
pinaforeLibSection =
    headingBDS "Pinafore" "Functions for working with Pinafore source code."
        $ pure
        $ namespaceBDS
            "Pinafore"
            [ headingBDS
                "Context"
                ""
                [ typeBDS
                    "Context"
                    "The context used for running `Interpreter`."
                    (qSomeGroundType @_ @QContext)
                    [ let
                        defaultloadModule :: Text -> QInterpreter (Maybe QDeclarations)
                        defaultloadModule _ = return Nothing
                        rtype :: ListType QDocSignature '[Text -> QInterpreter (Maybe QDeclarations)]
                        rtype =
                            ConsListType
                                ( ValueDocSignature @(Text -> QInterpreter (Maybe QDeclarations)) "loadModule" "" qType
                                    $ Just
                                    $ pure defaultloadModule
                                )
                                NilListType
                        qContextLoadModule :: QContext -> Text -> QInterpreter (Maybe QDeclarations)
                        qContextLoadModule (MkQContext lc) name = do
                            mqm <- runLoadModule (lcLoadModule lc) $ MkModuleName name
                            return $ fmap moduleDeclarations mqm
                        toLoadModule :: (Text -> QInterpreter (Maybe QDeclarations)) -> LoadModule
                        toLoadModule lm =
                            MkLoadModule $ \(MkModuleName name) -> do
                                msdocs <- lm name
                                for msdocs declarationsModule
                        fromQContext :: QContext -> Maybe (ListVProduct '[Text -> QInterpreter (Maybe QDeclarations)])
                        fromQContext qc =
                            Just $ listProductToVProduct (listTypeToVType rtype) (qContextLoadModule qc, ())
                        toQContext :: ListVProduct '[Text -> QInterpreter (Maybe QDeclarations)] -> QContext
                        toQContext lvp = let
                            (lm, ()) = listVProductToProduct lvp
                            lcLoadModule = toLoadModule lm
                            in MkQContext $ MkLibraryContext{..}
                        codec :: Codec QContext (ListVProduct '[Text -> QInterpreter (Maybe QDeclarations)])
                        codec = MkCodec fromQContext toQContext
                        in recordConsBDS "Mk" "" rtype codec
                    ]
                , namespaceBDS
                    "Context"
                    [ valBDS "get" "`The context for running the interpreter." getContext
                    , valBDS "this" "`!{this}: Context.Pinafore`  \nThe context at this point in source." thisContext
                    ]
                ]
            , headingBDS
                "Interpreter"
                ""
                [ typeBDS
                    "Interpreter"
                    "All parsing and type-checking takes place within `Interpreter`."
                    (qSomeGroundType @_ @QInterpreter)
                    []
                , namespaceBDS "Interpreter" $ monadEntries @Interpreter <> [valBDS "run" "" langRunInterpreter]
                ]
            , headingBDS
                "Scope"
                ""
                [ typeBDS "Scope" "" (qSomeGroundType @_ @QScope) []
                , namespaceBDS
                    "Scope"
                    [ valBDS "get" "`The current scope." getScope
                    , valBDS "this" "`!{this}: Scope.Pinafore`  \nThe scope at this point in source." thisScope
                    ]
                ]
            , headingBDS
                "Anchor"
                ""
                [ typeBDS "Anchor" "An anchor, for a point or property." (qSomeGroundType @_ @Anchor) []
                , hasSubtypeRelationBDS @Anchor @Showable Verify "" $ functionToShim "show" textShowable
                ]
            , headingBDS
                "Type"
                ""
                [ typeBDS "Type" "A (concrete nonpolar) Pinafore type." (qSomeGroundType @_ @LangType) []
                , hasSubtypeRelationBDS @LangType @Showable Verify "" $ functionToShim "show" textShowable
                ]
            , headingBDS
                "OpenType"
                ""
                [ typeBDS "OpenType" "A (concrete nonpolar) Pinafore type." (qSomeGroundType @_ @LangOpenType) []
                , hasSubtypeRelationBDS @(LangOpenType '(P, Q)) @LangType Verify ""
                    $ functionToShim "openLangTypeToType" openLangTypeToType
                , namespaceBDS "OpenType" [valBDS "unify" "Unify two `OpenType`s." langUnifyOpenTypes]
                ]
            , headingBDS
                "Expression"
                ""
                [ typeBDS
                    "Expression"
                    ""
                    (qSomeGroundType @_ @LangExpression)
                    [ valPatBDS
                        "Closed"
                        "Construct an `Expression` from a `Value`."
                        (MkLangExpression . qConstValue . unLangValue)
                        $ ImpureFunction
                        $ pure
                        $ \expr -> do
                            v <- qEvalExprMaybe $ unLangExpression expr
                            return (MkLangValue v, ())
                    ]
                , namespaceBDS
                    "Expression"
                    [ valBDS "interpret" "Interpret a Pinafore expression." interpretToExpression
                    , valBDS "eval" "Evaluate an `Expression`, or throw an error." evaluateLangExpression
                    ]
                ]
            , headingBDS
                "Value"
                ""
                [ typeBDS "Value" "" (qSomeGroundType @_ @LangValue) []
                , namespaceBDS
                    "Value"
                    [ valBDS "mk" "Create a `Value` from a `Type` and a value." mkLangValue
                    , valBDS "unify" "Unify a `Type` with a `Value`." langUnifyValue
                    , valBDS "interpret" "Interpret a Pinafore expression." interpretToValue
                    ]
                ]
            , headingBDS
                "Declarations"
                ""
                [ typeBDS "Declarations" "" (qSomeGroundType @_ @QDeclarations) []
                , namespaceBDS "Declarations"
                    $ monoidEntries @QDeclarations
                    <> [ valBDS "interpret" "Interpret a Pinafore module (list of declarations)." interpretModuleFromSource
                       , valBDS "apply" "Interpret within a given scope." langWithScope
                       , valBDS "bind" "Let-bind a name to a value." bindScope
                       ]
                ]
            ]

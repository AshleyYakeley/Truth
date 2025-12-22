{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Pinafore
    ( pinaforeLibSection
    )
where

import Text.Parsec.Pos

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

scopeLookup :: QScope -> Text -> Maybe QScopeItem
scopeLookup scope namet = do
    name <- fullNameFromStringMaybe $ unpack namet
    (_, qbi) <- bindingMapLookupInfo (scopeBindings scope) name
    return qbi

langRunInterpreter :: QContext -> QInterpreter A -> Action (Result QLocatedError A)
langRunInterpreter (MkQContext lc) ia = let
    ?library = lc
    in runInterpretResult $ runPinaforeScoped "<evaluate>" ia

langTypePositive :: LangOpenType '(p, q) -> QShimWit 'Positive p
langTypePositive (MkLangOpenType r t) = mapShimWit (MkPolarShim $ rangeContra r) $ nonpolarToPositive @QTypeSystem t

langTypeNegative :: LangOpenType '(p, q) -> QShimWit 'Negative q
langTypeNegative (MkLangOpenType r t) = mapShimWit (MkPolarShim $ rangeCo r) $ nonpolarToNegative @QTypeSystem t

runQTypeM :: QScope -> QTypeM --> Result QError
runQTypeM scope ma = mapResultFailure qTypeError $ runDolanTypeMEntries (toList $ scopeSubtypes scope) ma

langUnifyOpenTypes :: QScope -> LangOpenType '(A, TopType) -> LangOpenType '(BottomType, B) -> Result QError (A -> B)
langUnifyOpenTypes scope ta tb =
    fmap shimToFunction
        $ runQTypeM scope
        $ tsUnifyRigid @QTypeSystem (langTypePositive ta) (langTypeNegative tb)

-- LangValue
newtype LangValue = MkLangValue QValue

langValueExpression :: LangValue -> LangExpression
langValueExpression (MkLangValue val) = MkLangExpression $ qConstValue val

langExpressionValue :: LangExpression -> Maybe LangValue
langExpressionValue (MkLangExpression expr) = do
    val <- qEvalExprMaybe expr
    return $ MkLangValue val

instance HasQGroundType '[] LangValue where
    qGroundType =
        (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangValue)|]) "Value.Pinafore.")
            { qgtGreatestDynamicSupertype =
                simplePolyGreatestDynamicSupertype (qGroundType @_ @LangExpression) $ functionToShim "langExpressionValue" langExpressionValue
            }

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

interpretToType :: Text -> QInterpreter LangType
interpretToType src = do
    st <- parseNonpolarType src
    case st of
        MkSome t -> return $ MkLangType t

langUnifyValue :: QScope -> LangOpenType '(BottomType, A) -> LangValue -> Result QError A
langUnifyValue scope t (MkLangValue v) = runQTypeM scope $ tsUnifyValueTo @QTypeSystem (langTypeNegative t) v

langWithScope :: QDeclarations -> QInterpreter A -> QInterpreter A
langWithScope sdocs = withDeclarations sdocs

bindScope :: Text -> LangExpression -> QDeclarations
bindScope name (MkLangExpression expr@(MkSealedExpression t _)) = let
    fname = fromString $ unpack name
    doc :: DefDoc
    doc = MkDefDoc{docItem = ValueDocItem (pure $ fullNameRef fname) $ exprShow t, docDescription = ""}
    in declarations $ bindingInfosToScope $ pure $ (fname, MkQScopeItem fname doc $ ValueItem expr)

bindInterpreter :: Text -> LangExpression -> QInterpreter A -> QInterpreter A
bindInterpreter name expr = langWithScope $ bindScope name expr

interpretModuleFromSource :: Text -> QInterpreter QDeclarations
interpretModuleFromSource src = do
    m <- parseModule src
    return $ moduleDeclarations m

toLocated :: Text -> Int -> Int -> A -> Located A
toLocated n r c i = MkLocated (newPos (unpack n) r c) toText i

fromLocated :: Located A -> (Text, (Int, (Int, (A, ()))))
fromLocated (MkLocated spos _ item) = (pack $ sourceName spos, (sourceLine spos, (sourceColumn spos, (item, ()))))

itemValue :: QItem -> Maybe LangExpression
itemValue item = do
    bval <- getBoundValue item
    case bval of
        ValueBoundValue expr -> return $ MkLangExpression expr
        RecordBoundValue _ -> Nothing

langImply :: Text -> LangExpression -> LangExpression -> QInterpreter LangExpression
langImply n (MkLangExpression b) (MkLangExpression expr) = do
    expr1 <- qImply [(MkImplicitName $ MkName n, b)] expr
    return $ MkLangExpression expr1

instance HasQGroundType '[] PrecText where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily PrecText)|]) "PrecText."

newtype ToSource = MkToSource {toSource :: PrecText}
    deriving newtype ToText

instance HasQGroundType '[] ToSource where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ToSource)|]) "ToSource.Pinafore."

pinaforeLibSection :: LibraryStuff
pinaforeLibSection =
    headingBDS
        "Pinafore"
        "Functions for working with Pinafore source code."
        [ headingBDS
            "Located"
            ""
            [ typeBDS
                "Located"
                "Something located in textual source."
                (qSomeGroundType @_ @Located)
                [ valPatBDS
                    "Mk"
                    "Construct a `Located` from source, line, column, item."
                    toLocated
                    $ PureFunction
                    $ pure fromLocated
                ]
            , hasSubtypeRelationBDS
                @(Located Showable)
                @Showable
                Verify
                ""
                $ functionToShim "locatedShow"
                $ \(MkLocated spos _ s) -> PlainShowable $ showLocated spos $ showText s
            ]
        , headingBDS
            "PrecText"
            ""
            [ typeBDS
                "PrecText"
                "Text that puts parentheses in the right place."
                (qSomeGroundType @_ @PrecText)
                []
            , namespaceBDS
                "PrecText"
                $ monoidEntries @PrecText
                <> [ valBDS "fromTextPrec" "" textPrec
                   , valBDS "fromText" "" textToPrec
                   , valBDS "toTextPrec" "" precText
                   , valBDS "toText" "" (toText @PrecText)
                   ]
            ]
        , namespaceBDS
            "Pinafore"
            [ headingBDS
                "ToSource"
                ""
                [ typeBDS
                    "ToSource"
                    "Things that can be written as Pinafore expressions."
                    (qSomeGroundType @_ @ToSource)
                    [ valPatBDS "Mk" "" MkToSource
                        $ PureFunction
                        $ pure
                        $ \(MkToSource v) -> (v, ())
                    ]
                , valBDS "toSource" "" toSource
                , valBDS "toText" "" (toText @ToSource)
                ]
            , headingBDS
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
                                ( mkValueDocSignature @(Text -> QInterpreter (Maybe QDeclarations)) "loadModule" ""
                                    $ Just defaultloadModule
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
                    [ valBDS "get" "The context for running the interpreter." getContext
                    , valBDS "this" "`!{this.Context.Pinafore}: Context.Pinafore`  \nThe context at this point in source." thisContext
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
                , namespaceBDS "Interpreter"
                    $ monadEntries @Interpreter
                    <> [ hasSubtypeRelationBDS
                            @(Result QError A)
                            @(Interpreter A)
                            Verify
                            ""
                            $ functionToShim "resultInterpreter" fromResult
                       , valBDS "run" "" langRunInterpreter
                       ]
                ]
            , headingBDS
                "Item"
                ""
                [ typeBDS "Item" "" (qSomeGroundType @_ @QItem) []
                , namespaceBDS
                    "Item"
                    [ valBDS "value" "" itemValue
                    ]
                ]
            , headingBDS
                "ScopeItem"
                ""
                [ typeBDS "ScopeItem" "" (qSomeGroundType @_ @QScopeItem) []
                , namespaceBDS
                    "ScopeItem"
                    [ valBDS "item" "" siItem
                    , valBDS "description" "" $ toText . docDescription . siDocumentation
                    , valBDS "originalName" "" $ showText . siOriginalName
                    ]
                ]
            , headingBDS
                "Scope"
                ""
                [ typeBDS "Scope" "" (qSomeGroundType @_ @QScope) []
                , namespaceBDS
                    "Scope"
                    [ valBDS "get" "The current scope." getScope
                    , valBDS "this" "`!{this.Scope.Pinafore}: Scope.Pinafore`  \nThe scope at this point in source." thisScope
                    , valBDS "lookup" "Look up an item in this scope by full name." scopeLookup
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
                , namespaceBDS
                    "Type"
                    [valBDS "interpret" "Interpret a Pinafore type." interpretToType]
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
                "Error"
                ""
                [ typeBDS "Error" "" (qSomeGroundType @_ @QError) []
                , hasSubtypeRelationBDS @QError @Showable Verify "" $ functionToShim "show" $ PlainShowable . toText . showNamedText
                ]
            , headingBDS
                "Expression"
                ""
                [ typeBDS "Expression" "" (qSomeGroundType @_ @LangExpression) []
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
                , hasSubtypeRelationBDS @LangValue @LangExpression Verify ""
                    $ functionToShim "valueExpression" langValueExpression
                , namespaceBDS
                    "Value"
                    [ valBDS "mk" "Create a `Value` from a `Type` and a value." mkLangValue
                    , valBDS "unify" "Unify a `Type` with a `Value`." langUnifyValue
                    , valBDS "interpret" "Interpret a Pinafore value." interpretToValue
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
                       , valBDS "bind" "Let-bind a name to an expression." bindScope
                       ]
                ]
            , valBDS "bind" "Let-bind a name to a expression." bindInterpreter
            , valBDS "implyIn" "Set the value of an implicit variable for this expression.  \nName should not include the initial '?'." langImply
            ]
        ]

{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fconstraint-solver-iterations=20  #-}

module Pinafore.Language.Library.Defs
    ( EnA
    , qPositiveTypeDescription
    , qNegativeTypeDescription
    , headingBDT
    , headingBDS
    , namespaceBDS
    , valExprBDS
    , valBDS
    , typeBDS
    , typeBDS_
    , subtypeRelationBDS
    , hasSubtypeRelationBDS
    , valPatBDS
    , QDocSignature (..)
    , mkValueDocSignature
    , recordValueBDS
    , recordConsBDS
    , addNameInRootBDS
    , pickNamesInRootBDS
    , eqEntries
    , ordEntries
    , orderEntries
    , enumEntries
    , functorEntries
    , applicativeEntries
    , monadEntries
    , semigroupEntries
    , monoidEntries
    , sequenceEntries
    , elementsEntries
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

type EnA = MeetType Entity A

qPositiveShimWitDescription :: forall t. QShimWit 'Positive t -> NamedText
qPositiveShimWitDescription (MkShimWit w _) = exprShow w

qPositiveTypeDescription ::
    forall t.
    HasQType QPolyShim 'Positive t =>
    NamedText
qPositiveTypeDescription =
    case toPolarShimWit @Type @QShim @(QType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
    forall t.
    HasQType QPolyShim 'Negative t =>
    NamedText
qNegativeTypeDescription =
    case fromPolarShimWit @Type @QShim @(QType 'Negative) @t of
        MkShimWit w _ -> exprShow w

instance NamespaceConcat ScopeEntry where
    namespaceConcat nsn (BindScopeEntry fn xn b) = BindScopeEntry (namespaceConcat nsn fn) xn b
    namespaceConcat _ se = se

instance NamespaceConcat BindDoc where
    namespaceConcat nsn bd =
        bd{bdScopeEntry = namespaceConcat nsn $ bdScopeEntry bd, bdDoc = namespaceConcat nsn $ bdDoc bd}

nameInRoot :: FullNameRef -> FullNameRef
nameInRoot x = x{fnrSpace = RootNamespaceRef}

class AddNameInRoot t where
    addNameInRoot :: t -> t

instance AddNameInRoot t => AddNameInRoot (Maybe t) where
    addNameInRoot = fmap addNameInRoot

instance AddNameInRoot (NonEmpty FullNameRef) where
    addNameInRoot aa@(a :| _) = nameInRoot a :| toList aa

instance AddNameInRoot ScopeEntry where
    addNameInRoot (BindScopeEntry name oname f) =
        BindScopeEntry name (namespaceConcatFullName RootNamespace (nameInRoot name) : oname) f
    addNameInRoot e = e

instance AddNameInRoot DocItem where
    addNameInRoot x = runIdentity $ diNamesTraversal (Identity . addNameInRoot) x

instance AddNameInRoot DefDoc where
    addNameInRoot x = x{docItem = addNameInRoot $ docItem x}

instance AddNameInRoot BindDoc where
    addNameInRoot (MkBindDoc entries doc) = MkBindDoc (addNameInRoot entries) (addNameInRoot doc)

addNameInRootBDS :: LibraryStuff -> LibraryStuff
addNameInRootBDS bdt = fmap addNameInRoot bdt

pickNamesInRootBDS :: [FullNameRef] -> [LibraryStuff] -> [LibraryStuff]
pickNamesInRootBDS names =
    fmap
        $ fmap
        $ \d ->
            if (any (\name -> elem name $ bindDocNames d) names)
                then addNameInRoot d
                else d

headingBDT :: MarkdownText -> RawMarkdown -> [LibraryStuff] -> Tree BindDoc
headingBDT name desc tree = MkTree (MkBindDoc Nothing $ MkDefDoc (HeadingDocItem name) desc) $ mconcat tree

headingBDS :: MarkdownText -> RawMarkdown -> [LibraryStuff] -> LibraryStuff
headingBDS name desc tree = pureForest $ headingBDT name desc tree

namespaceBDS :: NamespaceRef -> [LibraryStuff] -> LibraryStuff
namespaceBDS name tree = namespaceConcat name $ mconcat tree

valExprBDS :: FullNameRef -> RawMarkdown -> QExpression -> LibraryStuff
valExprBDS name docDescription expr@(MkSealedExpression qt _) = let
    bdScopeEntry = pure $ BindScopeEntry name [] $ ValueItem expr
    diNames = pure name
    diType = qPositiveShimWitDescription qt
    docItem = ValueDocItem{..}
    bdDoc = MkDefDoc{..}
    in singleBindDoc MkBindDoc{..} []

valBDS ::
    forall t.
    HasQType QPolyShim 'Positive t =>
    FullNameRef ->
    RawMarkdown ->
    t ->
    LibraryStuff
valBDS name docDescription val = valExprBDS name docDescription $ qConstValue $ MkSomeOf qType val

newTypeParameter :: State [Name] Name
newTypeParameter = do
    nn <- get
    case nn of
        n : nr -> do
            put nr
            return n
        [] -> return "a"

getTypeParameter :: CCRVarianceType sv -> State [Name] (DocTypeParameter, Some (CCRPolarArgument QType 'Negative sv))
getTypeParameter CoCCRVarianceType = do
    vname <- newTypeParameter
    nameToTypeVarT vname $ \var ->
        return
            ( CoDocTypeParameter $ exprShow vname
            , MkSome $ CoCCRPolarArgument $ singleDolanType $ VarDolanSingularType var
            )
getTypeParameter ContraCCRVarianceType = do
    vname <- newTypeParameter
    nameToTypeVarT vname $ \var ->
        return
            ( ContraDocTypeParameter $ exprShow vname
            , MkSome $ ContraCCRPolarArgument $ singleDolanType $ VarDolanSingularType var
            )
getTypeParameter RangeCCRVarianceType = do
    vpname <- newTypeParameter
    vqname <- newTypeParameter
    nameToTypeVarT vpname $ \varp ->
        nameToTypeVarT vqname $ \varq ->
            return
                ( RangeDocTypeParameter (exprShow vpname) (exprShow vqname)
                , MkSome
                    $ RangeCCRPolarArgument
                        (singleDolanType $ VarDolanSingularType varp)
                        (singleDolanType $ VarDolanSingularType varq)
                )

getTypeParameters ::
    CCRVariancesType dv -> State [Name] ([DocTypeParameter], Some (CCRPolarArguments dv QType t 'Negative))
getTypeParameters NilListType = return ([], MkSome NilCCRArguments)
getTypeParameters (ConsListType t tt) = do
    (nt, sarg) <- getTypeParameter t
    case sarg of
        MkSome arg -> do
            (ntt, sargs) <- getTypeParameters tt
            case sargs of
                MkSome args -> return (nt : ntt, MkSome $ ConsCCRArguments arg args)

nameSupply :: [Name]
nameSupply = fmap (\c -> MkName $ pack [c]) ['a' .. 'z']

getGDSName :: QGroundType dv t -> Some (CCRPolarArguments dv QType t 'Negative) -> Maybe NamedText
getGDSName gt (MkSome params) =
    case qgtGreatestDynamicSupertype gt of
        NullPolyGreatestDynamicSupertype -> Nothing
        MkPolyGreatestDynamicSupertype f -> Just $ exprShow $ f params

typeBDS :: FullNameRef -> RawMarkdown -> QSomeGroundType -> [LibraryStuff] -> LibraryStuff
typeBDS name docDescription t@(MkSomeGroundType gt) bdChildren = let
    bdScopeEntry = pure $ BindScopeEntry name [] $ TypeItem t
    diNames = pure name
    (diParams, params) = evalState (getTypeParameters $ qgtVarianceType gt) nameSupply
    diStorable = isJust $ getGroundProperty storabilityProperty gt
    diGDS = getGDSName gt params
    diSynonym = Nothing
    docItem = TypeDocItem{..}
    bdDoc = MkDefDoc{..}
    in singleBindDoc MkBindDoc{..} $ namespaceConcat (RelativeNamespaceRef [fnrName name]) bdChildren

typeBDS_ ::
    forall (dv :: CCRVariances) (t :: CCRVariancesKind dv).
    HasQGroundType dv t =>
    FullNameRef ->
    RawMarkdown ->
    [LibraryStuff] ->
    LibraryStuff
typeBDS_ name doc children = typeBDS name doc (MkSomeGroundType $ qGroundType @dv @t) children

subtypeRelationBDS ::
    forall a b.
    TrustOrVerify ->
    RawMarkdown ->
    QGroundedShimWit 'Negative a ->
    QGroundedShimWit 'Positive b ->
    QShim a b ->
    LibraryStuff
subtypeRelationBDS trustme docDescription ta tb conv = let
    diSubtype = exprShow ta
    diSupertype = exprShow tb
    bdScopeEntry = pure $ SubtypeScopeEntry $ subtypeConversionEntry trustme Nothing ta tb $ pure conv
    docItem = SubtypeRelationDocItem{..}
    bdDoc = MkDefDoc{..}
    in singleBindDoc MkBindDoc{..} []

hasSubtypeRelationBDS ::
    forall a b.
    (HasQType QPolyShim 'Negative a, HasQType QPolyShim 'Positive b) =>
    TrustOrVerify ->
    RawMarkdown ->
    QShim a b ->
    LibraryStuff
hasSubtypeRelationBDS trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationBDS trustme doc ta tb conv

valPatBDS ::
    forall t v lt.
    (HasQType QPolyShim 'Positive t, HasQType QPolyShim 'Negative v, ToListShimWit QShim (QType 'Positive) lt) =>
    FullNameRef ->
    RawMarkdown ->
    t ->
    QPurityFunction v lt ->
    LibraryStuff
valPatBDS name docDescription val pat = let
    bdScopeEntry = pure $ BindScopeEntry name [] $ PatternConstructorItem (qConst val) $ qToPatternConstructor pat
    diNames = pure name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem{..}
    bdDoc = MkDefDoc{..}
    in pure MkBindDoc{..}

data QDocSignature (t :: Type)
    = ValueDocSignature
        Name
        RawMarkdown
        (QIsoShimWit 'Positive t)
        (Maybe (QOpenExpression t))

mkValueDocSignature ::
    forall t.
    HasQType QPolyIsoShim 'Positive t =>
    Name ->
    RawMarkdown ->
    Maybe t ->
    QDocSignature t
mkValueDocSignature name doc mdef = ValueDocSignature name doc qType $ fmap pure mdef

mapListProductShimWit ::
    forall shim w (tt :: [Type]).
    CartesianShim shim =>
    ListType (ShimWit shim w) tt ->
    ShimWit shim (ListProductType w) (ListProduct tt)
mapListProductShimWit NilListType = mkShimWit $ MkListProductType $ NilListType
mapListProductShimWit (ConsListType (MkShimWit w1 conv1) ss) =
    case mapListProductShimWit ss of
        MkShimWit (MkListProductType wr) convr ->
            MkShimWit (MkListProductType $ ConsListType w1 wr) $ pairShim conv1 convr

docSignatureToSignature :: QDocSignature --> PShimWit QIsoShim QSignature 'Positive
docSignatureToSignature (ValueDocSignature n _ (MkShimWit t iconv) p) =
    MkShimWit (ValueSignature Nothing n t $ fmap (fmap $ shimToFunction $ polarPolyIsoPositive iconv) p) iconv

recordValueBDS ::
    forall (params :: [Type]) (t :: Type).
    HasQType QPolyShim 'Positive t =>
    FullNameRef ->
    RawMarkdown ->
    ListType QDocSignature params ->
    (ListProduct params -> t) ->
    LibraryStuff
recordValueBDS name docDescription docsigs f = let
    posType :: QShimWit 'Positive t
    posType = qType
    dsToDoc :: forall a. QDocSignature a -> Tree BindDoc
    dsToDoc (ValueDocSignature n d t p) =
        pure $ MkBindDoc Nothing $ MkDefDoc (ValueSignatureDocItem n (exprShow t) (isJust p)) d
    diNames = pure name
    diType = exprShow posType
    docItem = ValuePatternDocItem{..}
    in case mapListProductShimWit $ mapListType docSignatureToSignature docsigs of
        MkShimWit (MkListProductType sigs) argconv -> let
            qrv :: QRecordValue
            qrv =
                MkQRecordValue sigs
                    $ constSealedFExpression
                    $ MkSomeFor (shimWitToDolan posType)
                    $ f
                    . shimToFunction (polarPolyIsoNegative argconv)
            bdScopeEntry = pure $ BindScopeEntry name [] $ RecordValueItem qrv
            bdDoc = MkDefDoc{..}
            in pureForest $ MkTree MkBindDoc{..} $ MkForest $ listTypeToList dsToDoc docsigs

recordConsBDS ::
    forall (t :: Type) (tt :: [Type]).
    (HasQGroundedType QPolyShim 'Positive t, HasQGroundedType QPolyShim 'Negative t) =>
    FullNameRef ->
    RawMarkdown ->
    ListType QDocSignature tt ->
    Codec t (ListVProduct tt) ->
    LibraryStuff
recordConsBDS name docDescription docsigs codec = let
    posType :: QGroundedShimWit 'Positive t
    posType = qGroundedType
    negType :: QGroundedShimWit 'Negative t
    negType = qGroundedType
    dsToDoc :: forall a. QDocSignature a -> Tree BindDoc
    dsToDoc (ValueDocSignature n d t p) =
        pure $ MkBindDoc Nothing $ MkDefDoc (ValueSignatureDocItem n (exprShow t) (isJust p)) d
    diNames = pure name
    diType = exprShow posType
    docItem = ValuePatternDocItem{..}
    in case mapListProductShimWit $ mapListType docSignatureToSignature docsigs of
        MkShimWit (MkListProductType sigs) argconv -> let
            MkIsomorphism pq qp = polarPolyIso argconv
            argCodec =
                bijectionCodec
                    $ MkIsomorphism
                        (listProductToVProduct (listTypeToVType sigs) . shimToFunction pq . listVProductToProduct)
                        (listProductToVProduct (listTypeToVType docsigs) . shimToFunction qp . listVProductToProduct)
            qrc :: QRecordConstructor
            qrc = MkQRecordConstructor (listTypeToVType sigs) posType negType $ argCodec . codec
            bdScopeEntry = pure $ BindScopeEntry name [] $ RecordConstructorItem qrc
            bdDoc = MkDefDoc{..}
            in pureForest $ MkTree MkBindDoc{..} $ MkForest $ listTypeToList dsToDoc docsigs

equivalenceEntries ::
    forall (a :: Type).
    (HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    Equivalence a ->
    [LibraryStuff]
equivalenceEntries equiv =
    [headingBDS "Eq" "" [valBDS "==" "Equal." $ equivalent equiv, valBDS "/=" "Not equal." $ notEquivalent equiv]]

eqEntries ::
    forall (a :: Type).
    (Eq a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
eqEntries = equivalenceEntries $ eqEquivalence @a

orderEntries ::
    forall (a :: Type).
    (HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    Order a ->
    RawMarkdown ->
    [LibraryStuff]
orderEntries order doc =
    equivalenceEntries (orderEquivalence order)
        <> [ headingBDS
                "Order"
                ""
                [ valBDS "order" "Order" $ order
                , valBDS "compare" doc $ compareOrder order
                , valBDS "<" "Strictly less." $ orderLT order
                , valBDS "<=" "Less or equal." $ orderLE order
                , valBDS ">" "Strictly greater." $ orderGT order
                , valBDS ">=" "Greater or equal." $ orderGE order
                , valBDS "lesser" "Lesser of two." $ orderLesser order
                , valBDS "greater" "Greater of two." $ orderGreater order
                , valBDS "least" "Least of a list." $ orderLeast order
                , valBDS "greatest" "Greatest of a list." $ orderGreatest order
                , valBDS "sort" "Sort list" $ orderSort order
                ]
           ]

ordEntries ::
    forall (a :: Type).
    (Ord a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
ordEntries = orderEntries (ordOrder @a) ""

enumEntries ::
    forall (a :: Type).
    (Enum a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
enumEntries = [headingBDS "Enum" "" [valBDS "pred" "Previous value." $ pred @a, valBDS "succ" "Next value." $ succ @a]]

functorEntries ::
    forall (f :: Type -> Type).
    ( Functor f
    , HasQType QPolyShim 'Positive (f A)
    , HasQType QPolyShim 'Negative (f A)
    , HasQType QPolyShim 'Positive (f B)
    ) =>
    [LibraryStuff]
functorEntries = [headingBDS "Functor" "" [valBDS "map" "" (fmap :: (A -> B) -> f A -> f B)]]

applicativeEntries ::
    forall (f :: Type -> Type).
    ( Applicative f
    , HasQType QPolyShim 'Negative (f TopType)
    , HasQType QPolyShim 'Negative (f ())
    , HasQType QPolyShim 'Positive (f ())
    , HasQType QPolyShim 'Positive (f A)
    , HasQType QPolyShim 'Negative (f A)
    , HasQType QPolyShim 'Positive (f B)
    , HasQType QPolyShim 'Negative (f B)
    , HasQType QPolyShim 'Positive (f C)
    , HasQType QPolyShim 'Positive (f (A, B))
    , HasQType QPolyShim 'Positive (f [B])
    , HasQType QPolyShim 'Negative (f (A -> B))
    ) =>
    [LibraryStuff]
applicativeEntries =
    functorEntries @f
        <> [ headingBDS
                "Applicative"
                ""
                [ valBDS "pure" "" (pure :: A -> f A)
                , valBDS "apply" "" ((<*>) :: f (A -> B) -> f A -> f B)
                , valBDS "liftA2" "" (liftA2 :: (A -> B -> C) -> f A -> f B -> f C)
                , valBDS "**" "" (liftA2 (,) :: f A -> f B -> f (A, B))
                , valBDS ">>" "" ((*>) :: f TopType -> f A -> f A)
                , valBDS "for_" "Perform on each value of a list." (for_ :: [A] -> (A -> f ()) -> f ())
                , valBDS "for" "Perform on each value of a list, returning a list." (for :: [A] -> (A -> f B) -> f [B])
                ]
           ]

monadEntries ::
    forall (f :: Type -> Type).
    ( Monad f
    , HasQType QPolyShim 'Negative (f TopType)
    , HasQType QPolyShim 'Negative (f ())
    , HasQType QPolyShim 'Positive (f ())
    , HasQType QPolyShim 'Positive (f A)
    , HasQType QPolyShim 'Negative (f A)
    , HasQType QPolyShim 'Positive (f B)
    , HasQType QPolyShim 'Negative (f B)
    , HasQType QPolyShim 'Positive (f C)
    , HasQType QPolyShim 'Positive (f (A, B))
    , HasQType QPolyShim 'Positive (f [B])
    , HasQType QPolyShim 'Negative (f (A -> B))
    ) =>
    [LibraryStuff]
monadEntries = applicativeEntries @f <> [headingBDS "Monad" "" [valBDS ">>=" "" ((>>=) :: f A -> (A -> f B) -> f B)]]

semigroupEntries ::
    forall (a :: Type).
    (Semigroup a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
semigroupEntries = [headingBDS "Semigroup" "" [valBDS "<>" "" $ (<>) @a, valBDS "concat1" "" $ sconcat @a]]

monoidEntries ::
    forall (a :: Type).
    (Monoid a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
monoidEntries =
    semigroupEntries @a <> [headingBDS "Monoid" "" [valBDS "empty" "" $ mempty @a, valBDS "concat" "" $ mconcat @a]]

sequenceEntries ::
    forall (a :: Type).
    (IsSequence a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a) =>
    [LibraryStuff]
sequenceEntries =
    [ headingBDS
        "Sequence"
        ""
        [ valBDS "length" "The number of elements." $ olengthNat @a
        , valBDS "section" "`section start len x` is the section of `x` beginning at `start` of length `len`." $ \start len (x :: a) ->
            takeNat len $ dropNat start x
        , valBDS "take" "Take the first n elements." $ takeNat @a
        , valBDS "drop" "Drop the first n elements." $ dropNat @a
        ]
    ]

elementsEntries ::
    forall (a :: Type).
    ( IsSequence a
    , MonoFilterable a
    , HasQType QPolyShim 'Positive a
    , HasQType QPolyShim 'Negative a
    , HasQType QPolyShim 'Positive (Element a)
    ) =>
    [LibraryStuff]
elementsEntries =
    sequenceEntries @a
        <> [ headingBDS
                "Elements"
                ""
                [ valBDS "takeWhile" "Take while the condition holds." $ takeWhile @a
                , valBDS "dropWhile" "Drop while the condition holds." $ dropWhile @a
                , valBDS "filter" "Filter for matching elements." $ ofilter @a
                , valBDS "sort" "Sort by an order." $ \(MkOrder order) -> sortBy @a order
                , valBDS "index" "Get an element by index." $ \s (n :: Natural) -> index @a s $ fromIntegral n
                ]
           ]

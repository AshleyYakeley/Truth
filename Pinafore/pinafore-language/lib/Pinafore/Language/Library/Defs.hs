{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fconstraint-solver-iterations=20  #-}

module Pinafore.Language.Library.Defs
    ( EnA
    , qPositiveTypeDescription
    , qNegativeTypeDescription
    , headingBDT
    , headingBDS
    , namespaceBDS
    , valWitBDS
    , valBDS
    , typeBDS
    , typeBDS_
    , subtypeRelationBDS
    , hasSubtypeRelationBDS
    , valPatBDS
    , QDocSignature(..)
    , mkValueDocSignature
    , recordValueBDS
    , recordConsBDS
    , specialFormBDS
    , addNameInRootBDS
    , pickNamesInRootBDS
    , eqEntries
    , ordEntries
    , lesser
    , greater
    , orderEntries
    , enumEntries
    , functorEntries
    , applicativeEntries
    , monadEntries
    , semigroupEntries
    , monoidEntries
    , sequenceEntries
    ) where

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
       forall t. HasQType QPolyShim 'Positive t
    => NamedText
qPositiveTypeDescription =
    case toPolarShimWit @Type @QShim @(QType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. HasQType QPolyShim 'Negative t
    => NamedText
qNegativeTypeDescription =
    case fromPolarShimWit @Type @QShim @(QType 'Negative) @t of
        MkShimWit w _ -> exprShow w

instance NamespaceConcat (ScopeEntry context) where
    namespaceConcat nsn (BindScopeEntry fn xn b) = BindScopeEntry (namespaceConcat nsn fn) xn b
    namespaceConcat _ se = se

instance NamespaceConcat (BindDoc context) where
    namespaceConcat nsn bd =
        bd {bdScopeEntry = namespaceConcat nsn $ bdScopeEntry bd, bdDoc = namespaceConcat nsn $ bdDoc bd}

nameInRoot :: FullNameRef -> FullNameRef
nameInRoot x = x {fnrSpace = RootNamespaceRef}

class AddNameInRoot t where
    addNameInRoot :: t -> t

instance AddNameInRoot t => AddNameInRoot (Maybe t) where
    addNameInRoot = fmap addNameInRoot

instance AddNameInRoot (NonEmpty FullNameRef) where
    addNameInRoot aa@(a :| _) = nameInRoot a :| toList aa

instance AddNameInRoot (ScopeEntry context) where
    addNameInRoot (BindScopeEntry name oname f) =
        BindScopeEntry name (namespaceConcatFullName RootNamespace (nameInRoot name) : oname) f
    addNameInRoot e = e

instance AddNameInRoot DocItem where
    addNameInRoot x = runIdentity $ diNamesTraversal (Identity . addNameInRoot) x

instance AddNameInRoot DefDoc where
    addNameInRoot x = x {docItem = addNameInRoot $ docItem x}

instance AddNameInRoot (BindDoc context) where
    addNameInRoot (MkBindDoc entries doc) = MkBindDoc (addNameInRoot entries) (addNameInRoot doc)

addNameInRootBDS :: LibraryStuff context -> LibraryStuff context
addNameInRootBDS bdt = fmap addNameInRoot bdt

pickNamesInRootBDS :: [FullNameRef] -> [LibraryStuff context] -> [LibraryStuff context]
pickNamesInRootBDS names =
    fmap $
    fmap $ \d ->
        if (any (\name -> elem name $ bindDocNames d) names)
            then addNameInRoot d
            else d

headingBDT :: MarkdownText -> RawMarkdown -> [LibraryStuff context] -> Tree (BindDoc context)
headingBDT name desc tree = MkTree (MkBindDoc Nothing $ MkDefDoc (HeadingDocItem name) desc) $ mconcat tree

headingBDS :: MarkdownText -> RawMarkdown -> [LibraryStuff context] -> LibraryStuff context
headingBDS name desc tree = pureForest $ headingBDT name desc tree

namespaceBDS :: NamespaceRef -> [LibraryStuff context] -> LibraryStuff context
namespaceBDS name tree = namespaceConcat name $ mconcat tree

valWitBDS ::
       forall context t.
       FullNameRef
    -> RawMarkdown
    -> QShimWit 'Positive t
    -> ((?qcontext :: context) => t)
    -> LibraryStuff context
valWitBDS name docDescription qt val = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \context -> let
            ?qcontext = context
            in ValueBinding $ qConstValue $ MkSomeOf qt val
    diNames = pure name
    diType = qPositiveShimWitDescription qt
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in singleBindDoc MkBindDoc {..} []

valBDS ::
       forall context t. HasQType QPolyShim 'Positive t
    => FullNameRef
    -> RawMarkdown
    -> ((?qcontext :: context) => t)
    -> LibraryStuff context
valBDS name docDescription = valWitBDS name docDescription qType

newTypeParameter :: State [Name] Name
newTypeParameter = do
    nn <- get
    case nn of
        n:nr -> do
            put nr
            return n
        [] -> return "a"

getTypeParameter :: CCRVarianceType sv -> State [Name] (DocTypeParameter, Some (CCRPolarArgument QType Negative sv))
getTypeParameter CoCCRVarianceType = do
    vname <- newTypeParameter
    nameToTypeVarT vname $ \var ->
        return
            ( CoDocTypeParameter $ exprShow vname
            , MkSome $ CoCCRPolarArgument $ singleDolanType $ VarDolanSingularType var)
getTypeParameter ContraCCRVarianceType = do
    vname <- newTypeParameter
    nameToTypeVarT vname $ \var ->
        return
            ( ContraDocTypeParameter $ exprShow vname
            , MkSome $ ContraCCRPolarArgument $ singleDolanType $ VarDolanSingularType var)
getTypeParameter RangeCCRVarianceType = do
    vpname <- newTypeParameter
    vqname <- newTypeParameter
    nameToTypeVarT vpname $ \varp ->
        nameToTypeVarT vqname $ \varq ->
            return
                ( RangeDocTypeParameter (exprShow vpname) (exprShow vqname)
                , MkSome $
                  RangeCCRPolarArgument
                      (singleDolanType $ VarDolanSingularType varp)
                      (singleDolanType $ VarDolanSingularType varq))

getTypeParameters ::
       CCRVariancesType dv -> State [Name] ([DocTypeParameter], Some (CCRPolarArguments dv QType t Negative))
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

getGDSName :: QGroundType dv t -> Some (CCRPolarArguments dv QType t Negative) -> Maybe NamedText
getGDSName gt (MkSome params) =
    case qgtGreatestDynamicSupertype gt of
        NullPolyGreatestDynamicSupertype -> Nothing
        MkPolyGreatestDynamicSupertype f -> Just $ exprShow $ f params

typeBDS ::
       forall context. FullNameRef -> RawMarkdown -> QSomeGroundType -> [LibraryStuff context] -> LibraryStuff context
typeBDS name docDescription t@(MkSomeGroundType gt) bdChildren = let
    bdScopeEntry = pure $ BindScopeEntry name [] $ \_ -> TypeBinding t
    diNames = pure name
    (diParams, params) = evalState (getTypeParameters $ qgtVarianceType gt) nameSupply
    diStorable = isJust $ getGroundProperty storabilityProperty gt
    diGDS = getGDSName gt params
    docItem = TypeDocItem {..}
    bdDoc = MkDefDoc {..}
    in singleBindDoc MkBindDoc {..} $ namespaceConcat (RelativeNamespaceRef [fnrName name]) bdChildren

typeBDS_ ::
       forall (dv :: CCRVariances) (t :: CCRVariancesKind dv) context. HasQGroundType dv t
    => FullNameRef
    -> RawMarkdown
    -> [LibraryStuff context]
    -> LibraryStuff context
typeBDS_ name doc children = typeBDS name doc (MkSomeGroundType $ qGroundType @dv @t) children

subtypeRelationBDS ::
       forall context a b.
       TrustOrVerify
    -> RawMarkdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QShim a b
    -> LibraryStuff context
subtypeRelationBDS trustme docDescription ta tb conv = let
    diSubtype = exprShow ta
    diSupertype = exprShow tb
    bdScopeEntry = pure $ SubtypeScopeEntry $ subtypeConversionEntry trustme Nothing ta tb $ pure conv
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in singleBindDoc MkBindDoc {..} []

hasSubtypeRelationBDS ::
       forall a b context. (HasQType QPolyShim 'Negative a, HasQType QPolyShim 'Positive b)
    => TrustOrVerify
    -> RawMarkdown
    -> QShim a b
    -> LibraryStuff context
hasSubtypeRelationBDS trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationBDS trustme doc ta tb conv

valPatBDS ::
       forall context t v lt.
       (HasQType QPolyShim 'Positive t, HasQType QPolyShim 'Negative v, ToListShimWit QShim (QType 'Positive) lt)
    => FullNameRef
    -> RawMarkdown
    -> t
    -> PurityFunction Maybe v (ListProduct lt)
    -> LibraryStuff context
valPatBDS name docDescription val pat = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \_ ->
            PatternConstructorBinding (qConstValue $ jmToValue val) $ qToPatternConstructor pat
    diNames = pure name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

data QDocSignature (t :: Type) =
    ValueDocSignature Name
                      RawMarkdown
                      (QIsoShimWit 'Positive t)
                      (Maybe (QOpenExpression t))

mkValueDocSignature ::
       forall t. HasQType QPolyIsoShim 'Positive t
    => Name
    -> RawMarkdown
    -> Maybe t
    -> QDocSignature t
mkValueDocSignature name doc mdef = ValueDocSignature name doc qType $ fmap pure mdef

mapListProductShimWit ::
       forall shim w (tt :: [Type]). CartesianShim shim
    => ListType (ShimWit shim w) tt
    -> ShimWit shim (ListProductType w) (ListProduct tt)
mapListProductShimWit NilListType = mkShimWit $ MkListProductType $ NilListType
mapListProductShimWit (ConsListType (MkShimWit w1 conv1) ss) =
    case mapListProductShimWit ss of
        MkShimWit (MkListProductType wr) convr ->
            MkShimWit (MkListProductType $ ConsListType w1 wr) $ pairShim conv1 convr

docSignatureToSignature :: QDocSignature --> PShimWit QIsoShim QSignature 'Positive
docSignatureToSignature (ValueDocSignature n _ (MkShimWit t iconv) p) =
    MkShimWit (ValueSignature Nothing n t $ fmap (fmap $ shimToFunction $ polarPolyIsoPositive iconv) p) iconv

recordValueBDS ::
       forall context (params :: [Type]) (t :: Type). (HasQType QPolyShim 'Positive t)
    => FullNameRef
    -> RawMarkdown
    -> ListType QDocSignature params
    -> (ListProduct params -> t)
    -> LibraryStuff context
recordValueBDS name docDescription docsigs f = let
    posType :: QShimWit 'Positive t
    posType = qType
    dsToDoc :: forall a. QDocSignature a -> Tree (BindDoc context)
    dsToDoc (ValueDocSignature n d t p) =
        pure $ MkBindDoc Nothing $ MkDefDoc (ValueSignatureDocItem n (exprShow t) (isJust p)) d
    diNames = pure name
    diType = exprShow posType
    docItem = ValuePatternDocItem {..}
    in case mapListProductShimWit $ mapListType docSignatureToSignature docsigs of
           MkShimWit (MkListProductType sigs) argconv -> let
               qrv :: QRecordValue
               qrv =
                   MkQRecordValue sigs $
                   constSealedFExpression $
                   MkSomeFor (shimWitToDolan posType) $ f . shimToFunction (polarPolyIsoNegative argconv)
               bdScopeEntry = pure $ BindScopeEntry name [] $ \_ -> RecordValueBinding qrv
               bdDoc = MkDefDoc {..}
               in pureForest $ MkTree MkBindDoc {..} $ MkForest $ listTypeToList dsToDoc docsigs

recordConsBDS ::
       forall (t :: Type) (tt :: [Type]) context.
       (HasQGroundedType QPolyShim 'Positive t, HasQGroundedType QPolyShim 'Negative t)
    => FullNameRef
    -> RawMarkdown
    -> ListType QDocSignature tt
    -> Codec t (ListVProduct tt)
    -> LibraryStuff context
recordConsBDS name docDescription docsigs codec = let
    posType :: QGroundedShimWit 'Positive t
    posType = qGroundedType
    negType :: QGroundedShimWit 'Negative t
    negType = qGroundedType
    dsToDoc :: forall a. QDocSignature a -> Tree (BindDoc context)
    dsToDoc (ValueDocSignature n d t p) =
        pure $ MkBindDoc Nothing $ MkDefDoc (ValueSignatureDocItem n (exprShow t) (isJust p)) d
    diNames = pure name
    diType = exprShow posType
    docItem = ValuePatternDocItem {..}
    in case mapListProductShimWit $ mapListType docSignatureToSignature docsigs of
           MkShimWit (MkListProductType sigs) argconv -> let
               MkIsomorphism pq qp = polarPolyIso argconv
               argCodec =
                   bijectionCodec $
                   MkIsomorphism
                       (listProductToVProduct (listTypeToVType sigs) . shimToFunction pq . listVProductToProduct)
                       (listProductToVProduct (listTypeToVType docsigs) . shimToFunction qp . listVProductToProduct)
               qrc :: QRecordConstructor
               qrc = MkQRecordConstructor (listTypeToVType sigs) posType negType $ argCodec . codec
               bdScopeEntry = pure $ BindScopeEntry name [] $ \_ -> RecordConstructorBinding qrc
               bdDoc = MkDefDoc {..}
               in pureForest $ MkTree MkBindDoc {..} $ MkForest $ listTypeToList dsToDoc docsigs

specialFormBDS ::
       forall context.
       FullNameRef
    -> RawMarkdown
    -> [NamedText]
    -> NamedText
    -> ((?qcontext :: context) => QSpecialForm)
    -> LibraryStuff context
specialFormBDS name docDescription diAnnotations diType sf = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \pc -> let
            ?qcontext = pc
            in SpecialFormBinding sf
    diNames = pure name
    docItem = SpecialFormDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

eqEntries ::
       forall context (a :: Type). (Eq a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
eqEntries = [valBDS "==" "Equal." $ (==) @a, valBDS "/=" "Not equal." $ (/=) @a]

ordEntries ::
       forall context (a :: Type). (Ord a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
ordEntries =
    eqEntries @context @a <>
    [ valBDS "order" "Order" $ compare @a
    , valBDS "<" "Strictly less." $ (<) @a
    , valBDS "<=" "Less or equal." $ (<=) @a
    , valBDS ">" "Strictly greater." $ (>) @a
    , valBDS ">=" "Greater or equal." $ (>=) @a
    , valBDS "min" "Lesser of two" $ min @a
    , valBDS "max" "Greater of two" $ max @a
    ]

lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f a b =
    case f a b of
        GT -> b
        _ -> a

greater :: (a -> a -> Ordering) -> a -> a -> a
greater f a b =
    case f a b of
        GT -> a
        _ -> b

orderEntries ::
       forall context (a :: Type). (Eq a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => (a -> a -> Ordering)
    -> RawMarkdown
    -> [LibraryStuff context]
orderEntries order doc =
    eqEntries @context @a <>
    [ valBDS "order" doc $ order
    , valBDS "<" "Strictly less." $ \x y -> order x y == LT
    , valBDS "<=" "Less or equal." $ \x y -> order x y /= GT
    , valBDS ">" "Strictly greater." $ \x y -> order x y == GT
    , valBDS ">=" "Greater or equal." $ \x y -> order x y /= LT
    , valBDS "min" "Lesser of two" $ lesser order
    , valBDS "max" "Greater of two" $ greater order
    ]

enumEntries ::
       forall context (a :: Type). (Enum a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
enumEntries = [valBDS "pred" "Previous value." $ pred @a, valBDS "succ" "Next value." $ succ @a]

functorEntries ::
       forall context (f :: Type -> Type).
       ( Functor f
       , HasQType QPolyShim 'Positive (f A)
       , HasQType QPolyShim 'Negative (f A)
       , HasQType QPolyShim 'Positive (f B)
       , HasQType QPolyShim 'Negative (f B)
       )
    => [LibraryStuff context]
functorEntries = [valBDS "map" "" (fmap :: (A -> B) -> f A -> f B)]

applicativeEntries ::
       forall context (f :: Type -> Type).
       ( Applicative f
       , HasQType QPolyShim 'Negative (f TopType)
       , HasQType QPolyShim 'Negative (f ())
       , HasQType QPolyShim 'Positive (f ())
       , HasQType QPolyShim 'Positive (f A)
       , HasQType QPolyShim 'Negative (f A)
       , HasQType QPolyShim 'Positive (f B)
       , HasQType QPolyShim 'Negative (f B)
       , HasQType QPolyShim 'Positive (f C)
       , HasQType QPolyShim 'Negative (f C)
       , HasQType QPolyShim 'Positive (f (A, B))
       , HasQType QPolyShim 'Positive (f [B])
       , HasQType QPolyShim 'Negative (f (A -> B))
       )
    => [LibraryStuff context]
applicativeEntries =
    functorEntries @context @f <>
    [ valBDS "pure" "" (pure :: A -> f A)
    , valBDS "ap" "" ((<*>) :: f (A -> B) -> f A -> f B)
    , valBDS "liftA2" "" (liftA2 :: (A -> B -> C) -> f A -> f B -> f C)
    , valBDS "**" "" (liftA2 (,) :: f A -> f B -> f (A, B))
    , valBDS ">>" "" ((*>) :: f TopType -> f A -> f A)
    , valBDS "for_" "Perform on each value of a list." (for_ :: [A] -> (A -> f ()) -> f ())
    , valBDS "for" "Perform on each value of a list, returning a list." (for :: [A] -> (A -> f B) -> f [B])
    ]

monadEntries ::
       forall context (f :: Type -> Type).
       ( Monad f
       , HasQType QPolyShim 'Negative (f TopType)
       , HasQType QPolyShim 'Negative (f ())
       , HasQType QPolyShim 'Positive (f ())
       , HasQType QPolyShim 'Positive (f A)
       , HasQType QPolyShim 'Negative (f A)
       , HasQType QPolyShim 'Positive (f B)
       , HasQType QPolyShim 'Negative (f B)
       , HasQType QPolyShim 'Positive (f C)
       , HasQType QPolyShim 'Negative (f C)
       , HasQType QPolyShim 'Positive (f (A, B))
       , HasQType QPolyShim 'Positive (f [B])
       , HasQType QPolyShim 'Negative (f (A -> B))
       )
    => [LibraryStuff context]
monadEntries = applicativeEntries @context @f <> [valBDS ">>=" "" ((>>=) :: f A -> (A -> f B) -> f B)]

semigroupEntries ::
       forall context (a :: Type). (Semigroup a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
semigroupEntries = [valBDS "<>" "" $ (<>) @a, valBDS "concat1" "" $ sconcat @a]

monoidEntries ::
       forall context (a :: Type). (Monoid a, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
monoidEntries = semigroupEntries @context @a <> [valBDS "empty" "" $ mempty @a, valBDS "concat" "" $ mconcat @a]

sequenceEntries ::
       forall context (a :: Type).
       (IsSequence a, Index a ~ Int, HasQType QPolyShim 'Positive a, HasQType QPolyShim 'Negative a)
    => [LibraryStuff context]
sequenceEntries =
    [ valBDS "length" "The number of elements." $ olength @a
    , valBDS "section" "`section start len x` is the section of `x` beginning at `start` of length `len`." $ \start len (x :: a) ->
          take len $ drop start x
    , valBDS "take" "Take the first n elements." (take :: Int -> a -> a)
    , valBDS "drop" "Drop the first n elements." (drop :: Int -> a -> a)
    ]

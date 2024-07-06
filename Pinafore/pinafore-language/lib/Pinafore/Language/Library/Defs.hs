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
    , subtypeRelationBDS
    , hasSubtypeRelationBDS
    , valPatBDS
    , QDocSignature(..)
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
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var

type EnA = MeetType Entity A

qPositiveShimWitDescription :: forall t. QShimWit 'Positive t -> NamedText
qPositiveShimWitDescription (MkShimWit w _) = exprShow w

qPositiveTypeDescription ::
       forall t. HasQType 'Positive t
    => NamedText
qPositiveTypeDescription =
    case toPolarShimWit @Type @(QPolyShim Type) @(QType 'Positive) @t of
        MkShimWit w _ -> exprShow w

qNegativeTypeDescription ::
       forall t. HasQType 'Negative t
    => NamedText
qNegativeTypeDescription =
    case fromPolarShimWit @Type @(QPolyShim Type) @(QType 'Negative) @t of
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
       forall context t. HasQType 'Positive t
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

subtypeRelationBDS ::
       forall context a b.
       TrustOrVerify
    -> RawMarkdown
    -> QGroundedShimWit 'Negative a
    -> QGroundedShimWit 'Positive b
    -> QPolyShim Type a b
    -> LibraryStuff context
subtypeRelationBDS trustme docDescription ta tb conv = let
    diSubtype = exprShow ta
    diSupertype = exprShow tb
    bdScopeEntry = pure $ SubtypeScopeEntry $ subtypeConversionEntry trustme Nothing ta tb $ pure conv
    docItem = SubtypeRelationDocItem {..}
    bdDoc = MkDefDoc {..}
    in singleBindDoc MkBindDoc {..} []

hasSubtypeRelationBDS ::
       forall a b context. (HasQType 'Negative a, HasQType 'Positive b)
    => TrustOrVerify
    -> RawMarkdown
    -> QPolyShim Type a b
    -> LibraryStuff context
hasSubtypeRelationBDS trustme doc conv = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeRelationBDS trustme doc ta tb conv

valPatBDS ::
       forall context t v lt.
       (HasQType 'Positive t, HasQType 'Negative v, ToListShimWit (QPolyShim Type) (QType 'Positive) lt)
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
                      (QType 'Positive t)
                      (Maybe (QOpenExpression t))

recordConsBDS ::
       forall (t :: Type) (tt :: [Type]) context. (HasQGroundedType 'Positive t, HasQGroundedType 'Negative t)
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
    famType :: SomeFamilialType
    famType =
        case posType of
            MkShimWit (MkDolanGroundedType t _) _ -> MkSomeFamilialType $ qgtFamilyType t
    dsToSig :: QDocSignature --> QSignature 'Positive
    dsToSig (ValueDocSignature n _ t p) = ValueSignature famType n t p
    dsToDoc :: forall a. QDocSignature a -> Tree (BindDoc context)
    dsToDoc (ValueDocSignature n d t p) =
        pure $ MkBindDoc Nothing $ MkDefDoc (ValueSignatureDocItem n (exprShow t) (isJust p)) d
    sigs :: ListVType (QSignature 'Positive) tt
    sigs = listTypeToVType $ mapListType dsToSig docsigs
    qrc :: QRecordConstructor
    qrc = MkQRecordConstructor sigs posType negType codec
    diNames = pure name
    diType = exprShow posType
    docItem = ValuePatternDocItem {..}
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
       forall context (a :: Type). (Eq a, HasQType 'Positive a, HasQType 'Negative a)
    => [LibraryStuff context]
eqEntries = [valBDS "==" "Equal." $ (==) @a, valBDS "/=" "Not equal." $ (/=) @a]

ordEntries ::
       forall context (a :: Type). (Ord a, HasQType 'Positive a, HasQType 'Negative a)
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
       forall context (a :: Type). (Eq a, HasQType 'Positive a, HasQType 'Negative a)
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
       forall context (a :: Type). (Enum a, HasQType 'Positive a, HasQType 'Negative a)
    => [LibraryStuff context]
enumEntries = [valBDS "pred" "Previous value." $ pred @a, valBDS "succ" "Next value." $ succ @a]

functorEntries ::
       forall context (f :: Type -> Type).
       ( Functor f
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       )
    => [LibraryStuff context]
functorEntries = [valBDS "map" "" (fmap :: (A -> B) -> f A -> f B)]

applicativeEntries ::
       forall context (f :: Type -> Type).
       ( Applicative f
       , HasQType 'Negative (f TopType)
       , HasQType 'Negative (f ())
       , HasQType 'Positive (f ())
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Positive (f [B])
       , HasQType 'Negative (f (A -> B))
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
       , HasQType 'Negative (f TopType)
       , HasQType 'Negative (f ())
       , HasQType 'Positive (f ())
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Positive (f [B])
       , HasQType 'Negative (f (A -> B))
       )
    => [LibraryStuff context]
monadEntries = applicativeEntries @context @f <> [valBDS ">>=" "" ((>>=) :: f A -> (A -> f B) -> f B)]

semigroupEntries ::
       forall context (a :: Type). (Semigroup a, HasQType 'Positive a, HasQType 'Negative a)
    => [LibraryStuff context]
semigroupEntries = [valBDS "<>" "" $ (<>) @a, valBDS "concat1" "" $ sconcat @a]

monoidEntries ::
       forall context (a :: Type). (Monoid a, HasQType 'Positive a, HasQType 'Negative a)
    => [LibraryStuff context]
monoidEntries = semigroupEntries @context @a <> [valBDS "empty" "" $ mempty @a, valBDS "concat" "" $ mconcat @a]

sequenceEntries ::
       forall context (a :: Type). (IsSequence a, Index a ~ Int, HasQType 'Positive a, HasQType 'Negative a)
    => [LibraryStuff context]
sequenceEntries =
    [ valBDS "length" "The number of elements." $ olength @a
    , valBDS "section" "`section start len x` is the section of `x` beginning at `start` of length `len`." $ \start len (x :: a) ->
          take len $ drop start x
    , valBDS "take" "Take the first n elements." (take :: Int -> a -> a)
    , valBDS "drop" "Drop the first n elements." (drop :: Int -> a -> a)
    ]

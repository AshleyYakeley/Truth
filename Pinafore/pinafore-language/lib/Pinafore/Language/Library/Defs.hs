{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fconstraint-solver-iterations=20  #-}

module Pinafore.Language.Library.Defs
    ( ScopeEntry(..)
    , BindDoc(..)
    , BindDocStuff
    , LibraryContents
    , libraryContentsEntries
    , libraryContentsDocumentation
    , LibraryModule(..)
    , EnA
    , qPositiveTypeDescription
    , qNegativeTypeDescription
    , headingBDT
    , headingBDS
    , namespaceBDS
    , valBDS
    , typeBDS
    , subtypeRelationBDS
    , hasSubtypeRelationBDS
    , valPatBDS
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
    ) where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DefDoc
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Text
import Shapes

data ScopeEntry context
    = BindScopeEntry FullNameRef
                     [FullName]
                     (context -> QInterpreterBinding)
    | SubtypeScopeEntry QSubtypeConversionEntry

instance Contravariant ScopeEntry where
    contramap ab (BindScopeEntry name oname f) = BindScopeEntry name oname $ \c -> f $ ab c
    contramap _ (SubtypeScopeEntry entry) = SubtypeScopeEntry entry

scopeEntryName :: ScopeEntry context -> Maybe FullNameRef
scopeEntryName (BindScopeEntry name _ _) = Just name
scopeEntryName (SubtypeScopeEntry _) = Nothing

data BindDoc context = MkBindDoc
    { bdScopeEntry :: Maybe (ScopeEntry context)
    , bdDoc :: DefDoc
    }

bindDocNames :: BindDoc context -> Maybe FullNameRef
bindDocNames bd = mapMaybe scopeEntryName $ bdScopeEntry bd

instance Contravariant BindDoc where
    contramap ab (MkBindDoc se d) = MkBindDoc (fmap (contramap ab) $ se) d

type BindDocStuff context = Forest (BindDoc context)

singleBindDoc :: BindDoc context -> [BindDocStuff context] -> BindDocStuff context
singleBindDoc bd tt = pureForest $ MkTree bd $ mconcat tt

type LibraryContents context = Forest (BindDoc context)

libraryContentsEntries :: LibraryContents context -> [BindDoc context]
libraryContentsEntries = toList

libraryContentsDocumentation :: LibraryContents context -> Forest DefDoc
libraryContentsDocumentation = fmap bdDoc

data LibraryModule context = MkLibraryModule
    { lmName :: ModuleName
    , lmContents :: LibraryContents context
    }

instance Contravariant LibraryModule where
    contramap ab (MkLibraryModule n c) = MkLibraryModule n $ fmap (contramap ab) c

type EnA = MeetType Entity A

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

addNameInRootBDS :: BindDocStuff context -> BindDocStuff context
addNameInRootBDS bdt = fmap addNameInRoot bdt

pickNamesInRootBDS :: [FullNameRef] -> [BindDocStuff context] -> [BindDocStuff context]
pickNamesInRootBDS names =
    fmap $
    fmap $ \d ->
        if (any (\name -> elem name $ bindDocNames d) names)
            then addNameInRoot d
            else d

headingBDT :: MarkdownText -> RawMarkdown -> [BindDocStuff context] -> Tree (BindDoc context)
headingBDT name desc tree = MkTree (MkBindDoc Nothing $ MkDefDoc (HeadingDocItem name) desc) $ mconcat tree

headingBDS :: MarkdownText -> RawMarkdown -> [BindDocStuff context] -> BindDocStuff context
headingBDS name desc tree = pureForest $ headingBDT name desc tree

namespaceBDS :: NamespaceRef -> [BindDocStuff context] -> BindDocStuff context
namespaceBDS name tree = namespaceConcat name $ mconcat tree

valBDS ::
       forall context t. HasQType 'Positive t
    => FullNameRef
    -> RawMarkdown
    -> ((?qcontext :: context) => t)
    -> BindDocStuff context
valBDS name docDescription val = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \context -> let
            ?qcontext = context
            in ValueBinding (qConstExprAny $ jmToValue val) Nothing
    diNames = pure name
    diType = qPositiveTypeDescription @t
    docItem = ValueDocItem {..}
    bdDoc = MkDefDoc {..}
    in singleBindDoc MkBindDoc {..} []

newTypeParameter :: State [Name] Name
newTypeParameter = do
    nn <- get
    case nn of
        n:nr -> do
            put nr
            return n
        [] -> return "a"

getTypeParameter :: CCRVarianceType a -> State [Name] SyntaxTypeParameter
getTypeParameter CoCCRVarianceType = do
    v <- newTypeParameter
    return $ PositiveSyntaxTypeParameter v
getTypeParameter ContraCCRVarianceType = do
    v <- newTypeParameter
    return $ NegativeSyntaxTypeParameter v
getTypeParameter RangeCCRVarianceType = do
    vn <- newTypeParameter
    vp <- newTypeParameter
    return $ RangeSyntaxTypeParameter vn vp

getTypeParameters :: [Name] -> CCRVariancesType dv -> [NamedText]
getTypeParameters supply dvt = fmap exprShow $ evalState (listTypeForList dvt getTypeParameter) supply

nameSupply :: [Name]
nameSupply = fmap (\c -> MkName $ pack [c]) ['a' .. 'z']

typeBDS ::
       forall context. FullNameRef -> RawMarkdown -> QSomeGroundType -> [BindDocStuff context] -> BindDocStuff context
typeBDS name docDescription t bdChildren = let
    bdScopeEntry = pure $ BindScopeEntry name [] $ \_ -> TypeBinding t
    diNames = pure name
    diParams =
        case t of
            MkSomeGroundType pt -> getTypeParameters nameSupply $ qgtVarianceType pt
    diStorable =
        case t of
            MkSomeGroundType gt -> isJust $ getGroundProperty storabilityProperty gt
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
    -> BindDocStuff context
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
    -> BindDocStuff context
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
    -> BindDocStuff context
valPatBDS name docDescription val pat = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat
    diNames = pure name
    diType = qPositiveTypeDescription @t
    docItem = ValuePatternDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

specialFormBDS ::
       forall context.
       FullNameRef
    -> RawMarkdown
    -> [NamedText]
    -> NamedText
    -> ((?qcontext :: context) => QSpecialForm)
    -> BindDocStuff context
specialFormBDS name docDescription params diType sf = let
    bdScopeEntry =
        pure $
        BindScopeEntry name [] $ \pc -> let
            ?qcontext = pc
            in SpecialFormBinding sf
    diNames = pure name
    diParams = params
    docItem = SpecialFormDocItem {..}
    bdDoc = MkDefDoc {..}
    in pure MkBindDoc {..}

eqEntries ::
       forall context (a :: Type). (Eq a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocStuff context]
eqEntries = [valBDS "==" "Equal." $ (==) @a, valBDS "/=" "Not equal." $ (/=) @a]

ordEntries ::
       forall context (a :: Type). (Ord a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocStuff context]
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
    -> [BindDocStuff context]
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
    => [BindDocStuff context]
enumEntries = [valBDS "pred" "Previous value." $ pred @a, valBDS "succ" "Next value." $ succ @a]

functorEntries ::
       forall context (f :: Type -> Type).
       ( Functor f
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       )
    => [BindDocStuff context]
functorEntries = [valBDS "map" "" (fmap :: (A -> B) -> f A -> f B)]

applicativeEntries ::
       forall context (f :: Type -> Type).
       ( Applicative f
       , HasQType 'Negative (f TopType)
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Negative (f (A -> B))
       )
    => [BindDocStuff context]
applicativeEntries =
    functorEntries @context @f <>
    [ valBDS "pure" "" (pure :: A -> f A)
    , valBDS "ap" "" ((<*>) :: f (A -> B) -> f A -> f B)
    , valBDS "liftA2" "" (liftA2 :: (A -> B -> C) -> f A -> f B -> f C)
    , valBDS "**" "" (liftA2 (,) :: f A -> f B -> f (A, B))
    , valBDS ">>" "" ((*>) :: f TopType -> f A -> f A)
    ]

monadEntries ::
       forall context (f :: Type -> Type).
       ( Monad f
       , HasQType 'Negative (f TopType)
       , HasQType 'Positive (f A)
       , HasQType 'Negative (f A)
       , HasQType 'Positive (f B)
       , HasQType 'Negative (f B)
       , HasQType 'Positive (f C)
       , HasQType 'Negative (f C)
       , HasQType 'Positive (f (A, B))
       , HasQType 'Negative (f (A -> B))
       )
    => [BindDocStuff context]
monadEntries = applicativeEntries @context @f <> [valBDS ">>=" "" ((>>=) :: f A -> (A -> f B) -> f B)]

semigroupEntries ::
       forall context (a :: Type). (Semigroup a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocStuff context]
semigroupEntries = [valBDS "<>" "" $ (<>) @a, valBDS "concat1" "" $ sconcat @a]

monoidEntries ::
       forall context (a :: Type). (Monoid a, HasQType 'Positive a, HasQType 'Negative a)
    => [BindDocStuff context]
monoidEntries = semigroupEntries @context @a <> [valBDS "empty" "" $ mempty @a, valBDS "concat" "" $ mconcat @a]

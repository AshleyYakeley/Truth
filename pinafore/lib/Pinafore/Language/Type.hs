module Pinafore.Language.Type where

import GHC.TypeLits
import Pinafore.Language.Order
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Table (Point)
import Pinafore.Types
import Prelude (Bounded(..))
import Shapes
import Truth.Core

import Language.Expression.Type.Dolan
import Language.Expression.Type.Soft.Var

type Func a b = a -> b

class ExprShow t where
    exprShowPrec :: t -> (Text, Int)

exprPrecShow :: ExprShow t => Int -> t -> Text
exprPrecShow c t =
    case exprShowPrec t of
        (s, p) ->
            if c < p
                then "(" <> s <> ")"
                else s

exprShow :: ExprShow t => t -> Text
exprShow = exprPrecShow maxBound

newtype PinaforeMutableReference baseedit t =
    MkPinaforeMutableReference (PinaforeLensValue baseedit (WholeEdit t))

instance IsoVariant (PinaforeMutableReference baseedit) where
    isoMap ab ba (MkPinaforeMutableReference lens) =
        MkPinaforeMutableReference $ bijectionWholeEditLens (MkBijection ab ba) . lens

type PinaforeConstReference = WholeEditFunction

newtype PinaforeMutableSet baseedit t =
    MkPinaforeMutableSet (PinaforeLensValue baseedit (FiniteSetEdit t))

instance IsoVariant (PinaforeMutableSet baseedit) where
    isoMap ab ba (MkPinaforeMutableSet lens) =
        MkPinaforeMutableSet $ bijectionFiniteSetEditLens (MkBijection ab ba) . lens

newtype PinaforeConstSet baseedit t =
    MkPinaforeConstSet (PinaforeFunctionValue baseedit (FiniteSet t))

instance Functor (PinaforeConstSet baseedit) where
    fmap ab (MkPinaforeConstSet ef) = MkPinaforeConstSet $ wholeEditFunction (fmap ab) . ef

instance IsoVariant (PinaforeConstSet baseedit)

type QMorphism baseedit a b = PinaforeLensMorphism baseedit a b

newtype Entity (name :: Symbol) =
    MkEntity Point

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('SVar') for type variables.
data PinaforeType (baseedit :: Type) (mpolarity :: Maybe TypePolarity) (t :: Type) where
    LimitPinaforeType :: IsTypePolarity polarity => PinaforeType baseedit ('Just polarity) (LimitType polarity)
    JoinMeetPinaforeType
        :: IsTypePolarity polarity
        => PinaforeType baseedit ('Just polarity) a
        -> PinaforeType baseedit ('Just polarity) b
        -> PinaforeType baseedit ('Just polarity) (JoinMeetType polarity a b)
    FunctionPinaforeType
        :: PinaforeType baseedit (MInvertPolarity mpolarity) a
        -> PinaforeType baseedit mpolarity b
        -> PinaforeType baseedit mpolarity (Func a b)
    ListPinaforeType :: PinaforeType baseedit mpolarity a -> PinaforeType baseedit mpolarity [a]
    PairPinaforeType
        :: PinaforeType baseedit mpolarity a
        -> PinaforeType baseedit mpolarity b
        -> PinaforeType baseedit mpolarity (a, b)
    MutableReferencePinaforeType
        :: PinaforeType baseedit 'Nothing a -> PinaforeType baseedit mpolarity (PinaforeMutableReference baseedit a)
    ConstReferencePinaforeType
        :: PinaforeType baseedit mpolarity a -> PinaforeType baseedit mpolarity (PinaforeConstReference baseedit a)
    MutableSetPinaforeType
        :: PinaforeType baseedit 'Nothing a -> PinaforeType baseedit mpolarity (PinaforeMutableSet baseedit a)
    ConstSetPinaforeType
        :: PinaforeType baseedit mpolarity a -> PinaforeType baseedit mpolarity (PinaforeConstSet baseedit a)
    MorphismPinaforeType
        :: PinaforeType baseedit 'Nothing a
        -> PinaforeType baseedit 'Nothing b
        -> PinaforeType baseedit mpolarity (QMorphism baseedit a b)
    InverseMorphismPinaforeType
        :: PinaforeType baseedit 'Nothing a
        -> PinaforeType baseedit 'Nothing b
        -> PinaforeType baseedit mpolarity (QMorphism baseedit a b)
    GroundPinaforeType :: GroundType baseedit t -> PinaforeType baseedit mpolarity t
    VarPinaforeType :: SymbolWitness name -> PinaforeType baseedit mpolarity (SVar name)

polarisePinaforeType :: PinaforeType baseedit 'Nothing t -> PinaforeType baseedit ('Just polarity) t
polarisePinaforeType (FunctionPinaforeType ta tb) =
    FunctionPinaforeType (polarisePinaforeType ta) (polarisePinaforeType tb)
polarisePinaforeType (ListPinaforeType t) = ListPinaforeType $ polarisePinaforeType t
polarisePinaforeType (PairPinaforeType ta tb) = PairPinaforeType (polarisePinaforeType ta) (polarisePinaforeType tb)
polarisePinaforeType (MutableReferencePinaforeType t) = MutableReferencePinaforeType t
polarisePinaforeType (ConstReferencePinaforeType t) = ConstReferencePinaforeType $ polarisePinaforeType t
polarisePinaforeType (MutableSetPinaforeType t) = MutableSetPinaforeType t
polarisePinaforeType (ConstSetPinaforeType t) = ConstSetPinaforeType $ polarisePinaforeType t
polarisePinaforeType (MorphismPinaforeType ta tb) = MorphismPinaforeType ta tb
polarisePinaforeType (InverseMorphismPinaforeType ta tb) = InverseMorphismPinaforeType ta tb
polarisePinaforeType (GroundPinaforeType t) = GroundPinaforeType t
polarisePinaforeType (VarPinaforeType i) = VarPinaforeType i

renamePinaforeTypeVars ::
       (String -> String)
    -> PinaforeType baseedit mpolarity t1
    -> (forall t2. PinaforeType baseedit mpolarity t2 -> Bijection t1 t2 -> r)
    -> r
renamePinaforeTypeVars _ LimitPinaforeType cont = cont LimitPinaforeType id
renamePinaforeTypeVars sf t@(JoinMeetPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb ->
            cont (JoinMeetPinaforeType ta' tb') $ let
                bijab ::
                       forall polarity. IsTypePolarity polarity
                    => PinaforeType _ ('Just polarity) _
                    -> _
                bijab _ = jmBiMap @polarity bija bijb
                in bijab t
renamePinaforeTypeVars sf (FunctionPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb -> cont (FunctionPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars sf (ListPinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ListPinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (PairPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb -> cont (PairPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars sf (MutableReferencePinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (MutableReferencePinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (ConstReferencePinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ConstReferencePinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (MutableSetPinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (MutableSetPinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (ConstSetPinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ConstSetPinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (MorphismPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb -> cont (MorphismPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars sf (InverseMorphismPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb ->
            cont (InverseMorphismPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars _ (GroundPinaforeType t) cont = cont (GroundPinaforeType t) id
renamePinaforeTypeVars sf (VarPinaforeType namewit1) cont =
    renameSVar sf namewit1 $ \namewit2 bij -> cont (VarPinaforeType namewit2) bij

getTypeVars :: PinaforeType baseedit mpolarity t -> [String]
getTypeVars LimitPinaforeType = mempty
getTypeVars (JoinMeetPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (FunctionPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (ListPinaforeType t) = getTypeVars t
getTypeVars (PairPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (MutableReferencePinaforeType t) = getTypeVars t
getTypeVars (ConstReferencePinaforeType t) = getTypeVars t
getTypeVars (MutableSetPinaforeType t) = getTypeVars t
getTypeVars (ConstSetPinaforeType t) = getTypeVars t
getTypeVars (MorphismPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (InverseMorphismPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (GroundPinaforeType _) = mempty
getTypeVars (VarPinaforeType swit) = pure $ fromSymbolWitness swit

data Biunification baseedit =
    forall p q. MkBiunification (PinaforeType baseedit ('Just 'PositivePolarity) p)
                                (PinaforeType baseedit ('Just 'NegativePolarity) q)

instance Semigroup (Biunification baseedit) where
    (MkBiunification ap aq) <> (MkBiunification bp bq) =
        MkBiunification (JoinMeetPinaforeType ap bp) (JoinMeetPinaforeType aq bq)

instance Monoid (Biunification baseedit) where
    mempty = MkBiunification LimitPinaforeType LimitPinaforeType

type BiunificationTable baseedit = String -> Biunification baseedit

type BiunifierM baseedit = WriterT (BiunificationTable baseedit) (Result Text)

tellBiunification :: SymbolWitness name -> Biunification baseedit -> BiunifierM baseedit ()
tellBiunification swit u =
    tell $ \s ->
        if s == fromSymbolWitness swit
            then u
            else mempty

isPinaforeSameType ::
       PinaforeType baseedit 'Nothing p -> PinaforeType baseedit 'Nothing q -> BiunifierM baseedit (Bijection p q)
isPinaforeSameType tp tq = do
    pq <- isPinaforeSubtype (polarisePinaforeType tp) (polarisePinaforeType tq)
    qp <- isPinaforeSubtype (polarisePinaforeType tq) (polarisePinaforeType tp)
    return $ MkBijection pq qp

isPinaforeSubtype ::
       PinaforeType baseedit ('Just 'PositivePolarity) p
    -> PinaforeType baseedit ('Just 'NegativePolarity) q
    -> BiunifierM baseedit (p -> q)
isPinaforeSubtype LimitPinaforeType _ = return never
isPinaforeSubtype _ LimitPinaforeType = return $ \_ -> ()
isPinaforeSubtype (JoinMeetPinaforeType tp1 tp2) tq = do
    pq1 <- isPinaforeSubtype tp1 tq
    pq2 <- isPinaforeSubtype tp2 tq
    return $ \(MkJoinType ep) -> either pq1 pq2 ep
isPinaforeSubtype tp (JoinMeetPinaforeType tq1 tq2) = do
    pq1 <- isPinaforeSubtype tp tq1
    pq2 <- isPinaforeSubtype tp tq2
    return $ \p -> MkMeetType (pq1 p, pq2 p)
isPinaforeSubtype (FunctionPinaforeType tpa tpb) (FunctionPinaforeType tqa tqb) = do
    bb <- isPinaforeSubtype tpb tqb
    aa <- isPinaforeSubtype tqa tpa
    return $ \pp -> bb . pp . aa
isPinaforeSubtype (ListPinaforeType tp) (ListPinaforeType tq) = do
    pq <- isPinaforeSubtype tp tq
    return $ fmap pq
isPinaforeSubtype (PairPinaforeType tpa tpb) (PairPinaforeType tqa tqb) = do
    aa <- isPinaforeSubtype tpa tqa
    bb <- isPinaforeSubtype tpb tqb
    return $ \(pa, pb) -> (aa pa, bb pb)
isPinaforeSubtype (MutableReferencePinaforeType tp) (MutableReferencePinaforeType tq) = do
    pq <- isPinaforeSameType tp tq
    return $ biIsoMap pq
isPinaforeSubtype (ConstReferencePinaforeType tp) (ConstReferencePinaforeType tq) = do
    pq <- isPinaforeSubtype tp tq
    return $ fmap pq
isPinaforeSubtype (MutableSetPinaforeType tp) (MutableSetPinaforeType tq) = do
    pq <- isPinaforeSameType tp tq
    return $ biIsoMap pq
isPinaforeSubtype (ConstSetPinaforeType tp) (ConstSetPinaforeType tq) = do
    pq <- isPinaforeSubtype tp tq
    return $ fmap pq
isPinaforeSubtype (MorphismPinaforeType tpa tpb) (MorphismPinaforeType tqa tqb) = do
    aa <- isPinaforeSameType tpa tqa
    bb <- isPinaforeSameType tpb tqb
    return $ \pp -> biIsoMap' aa $ biIsoMap bb $ pp
isPinaforeSubtype (InverseMorphismPinaforeType tpa tpb) (InverseMorphismPinaforeType tqa tqb) = do
    aa <- isPinaforeSameType tpa tqa
    bb <- isPinaforeSameType tpb tqb
    return $ \pp -> biIsoMap' aa $ biIsoMap bb $ pp
isPinaforeSubtype (GroundPinaforeType tp) (GroundPinaforeType tq) = lift $ isSubtype tp tq
isPinaforeSubtype (VarPinaforeType swit) tq = do
    tellBiunification swit $ MkBiunification LimitPinaforeType tq
    return unsafeFromSVar
isPinaforeSubtype tp (VarPinaforeType swit) = do
    tellBiunification swit $ MkBiunification tp LimitPinaforeType
    return unsafeToSVar
isPinaforeSubtype tp tq = fail $ unpack $ "cannot match " <> exprShow tp <> " with " <> exprShow tq

simplifyType ::
       PinaforeType baseedit mpolarity a -> (forall b. PinaforeType baseedit mpolarity b -> Bijection a b -> r) -> r
simplifyType (JoinMeetPinaforeType LimitPinaforeType (tb :: PinaforeType _ ('Just polarity) _)) cont =
    cont tb $ jmLeftIdentity @polarity
simplifyType (JoinMeetPinaforeType ta (LimitPinaforeType :: PinaforeType _ ('Just polarity) _)) cont =
    cont ta $ jmRightIdentity @polarity
simplifyType t cont = cont t id

instance ExprShow (PinaforeType baseedit mpolarity t) where
    exprShowPrec t@LimitPinaforeType = let
        s :: forall polarity. IsTypePolarity polarity
          => PinaforeType _ ('Just polarity) _
          -> Text
        s _ = showLimitType @polarity
        in (s t, 0)
    exprShowPrec t@(JoinMeetPinaforeType ta tb) = let
        s :: forall polarity. IsTypePolarity polarity
          => PinaforeType _ ('Just polarity) _
          -> Text
        s _ = showJoinMeetType @polarity
        in (exprPrecShow 2 ta <> " " <> s t <> " " <> exprPrecShow 2 tb, 3)
    exprShowPrec (FunctionPinaforeType ta tb) = (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
    exprShowPrec (ListPinaforeType ta) = ("[" <> exprShow ta <> "]", 0)
    exprShowPrec (PairPinaforeType ta tb) = ("(" <> exprShow ta <> ", " <> exprShow tb <> ")", 0)
    exprShowPrec (MutableReferencePinaforeType ta) = ("MRef " <> exprPrecShow 0 ta, 2)
    exprShowPrec (ConstReferencePinaforeType ta) = ("CRef " <> exprPrecShow 0 ta, 2)
    exprShowPrec (MutableSetPinaforeType ta) = ("MSet " <> exprPrecShow 0 ta, 2)
    exprShowPrec (ConstSetPinaforeType ta) = ("CSet " <> exprPrecShow 0 ta, 2)
    exprShowPrec (MorphismPinaforeType ta tb) = (exprPrecShow 1 ta <> " ~> " <> exprPrecShow 1 tb, 2)
    exprShowPrec (InverseMorphismPinaforeType ta tb) = (exprPrecShow 1 tb <> " <~ " <> exprPrecShow 1 ta, 2)
    exprShowPrec (GroundPinaforeType t) = exprShowPrec t
    exprShowPrec (VarPinaforeType namewit) = ("a" <> pack (fromSymbolWitness namewit), 0)

data GroundType (baseedit :: Type) (t :: Type) where
    ActionGroundType :: GroundType baseedit (QAction baseedit)
    OrderGroundType :: GroundType baseedit (QOrder baseedit)
    UserInterfaceGroundType :: GroundType baseedit (UISpec (ConstEdit Point) baseedit)
    LiteralGroundType :: LiteralType t -> GroundType baseedit (Maybe t)
    PointGroundType :: KnownSymbol name => GroundType baseedit (Entity name)

gtNameProxy :: GroundType _ (Entity name) -> Proxy name
gtNameProxy _ = Proxy

class IsSubtype w where
    isSubtype :: w a -> w b -> Result Text (a -> b)

instance IsSubtype (GroundType baseedit) where
    isSubtype ActionGroundType ActionGroundType = return id
    isSubtype OrderGroundType OrderGroundType = return id
    isSubtype UserInterfaceGroundType UserInterfaceGroundType = return id
    isSubtype (LiteralGroundType ta) (LiteralGroundType tb) = do
        ab <- isSubtype ta tb
        return $ fmap ab
    isSubtype ta@PointGroundType tb@PointGroundType
        | Just Refl <- sameSymbol (gtNameProxy ta) (gtNameProxy tb) = return id
    isSubtype ta tb = FailureResult $ "cannot match " <> exprShow ta <> " with " <> exprShow tb

instance ExprShow (GroundType baseedit t) where
    exprShowPrec ActionGroundType = ("Action", 0)
    exprShowPrec OrderGroundType = ("Order", 0)
    exprShowPrec UserInterfaceGroundType = ("UI", 0)
    exprShowPrec (LiteralGroundType t) = exprShowPrec t
    exprShowPrec t@PointGroundType = let
        s :: forall name. KnownSymbol name
          => GroundType _ (Entity name)
          -> String
        s _ = symbolVal (Proxy :: Proxy name)
        in (pack $ s t, 0)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    BottomLiteralType :: LiteralType BottomType

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec BottomLiteralType = ("LiteralBottom", 0)

instance IsSubtype LiteralType where
    isSubtype LiteralLiteralType LiteralLiteralType = return id
    isSubtype TextLiteralType LiteralLiteralType = return toLiteral
    isSubtype NumberLiteralType LiteralLiteralType = return toLiteral
    isSubtype TextLiteralType TextLiteralType = return id
    isSubtype NumberLiteralType NumberLiteralType = return id
    isSubtype BottomLiteralType _ = return never
    isSubtype ta tb = FailureResult $ "cannot match " <> exprShow ta <> " with " <> exprShow tb
{-
getRenaming :: [String] -> [String] -> String -> String
getRenaming = ff

applyPinaforeType :: forall baseedit f x r. PinaforeType baseedit ('Just 'PositivePolarity) f -> PinaforeType baseedit ('Just 'PositivePolarity) x -> (forall fx. PinaforeType baseedit ('Just 'PositivePolarity) fx -> (f -> x -> fx) -> r) -> Result Text r
applyPinaforeType ft xt cont = let
    fvars = getTypeVars ft
    xvars = getTypeVars xt
    renamer = getRenaming fvars xvars
    in renamePinaforeTypeVars renamer xt $ \xt' xx' -> do
        (pq,table) <- runWriterT $ isPinaforeSubtype ft $ FunctionPinaforeType xt' $ VarPinaforeType $ MkSymbolWitness @""
        return $ cont foo $ \f x -> foo' $ pq f $ biForwards xx' x
-}

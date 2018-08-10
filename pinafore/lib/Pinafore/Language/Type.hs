module Pinafore.Language.Type where

import GHC.TypeLits
import Language.Expression.Dolan.Polarity
import Language.Expression.Dolan.TypeRange
import Language.Expression.UVar
import Pinafore.Language.Order
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Table (Point)
import Pinafore.Types
import Prelude (Bounded(..))
import Shapes
import Truth.Core

type Func a b = a -> b

class ExprShow t where
    exprShowPrec :: t -> (Text, Int)

precShow :: Int -> (Text, Int) -> Text
precShow c (s, p)
    | c < p = "(" <> s <> ")"
precShow _ (s, _) = s

exprPrecShow :: ExprShow t => Int -> t -> Text
exprPrecShow c t = precShow c $ exprShowPrec t

exprShow :: ExprShow t => t -> Text
exprShow = exprPrecShow maxBound

data PinaforeMutableReference baseedit pq =
    forall t. MkPinaforeMutableReference (TypeRange t pq)
                                         (PinaforeLensValue baseedit (WholeEdit t))

instance IsoMapTypeRange (PinaforeMutableReference baseedit)

instance MapTypeRange (PinaforeMutableReference baseedit) where
    mapTypeRange f (MkPinaforeMutableReference r lens) = MkPinaforeMutableReference (mapTypeRange f r) lens

type PinaforeConstReference = WholeEditFunction

data PinaforeMutableSet baseedit pq =
    forall t. MkPinaforeMutableSet (TypeRange t pq)
                                   (PinaforeLensValue baseedit (FiniteSetEdit t))

instance IsoMapTypeRange (PinaforeMutableSet baseedit)

instance MapTypeRange (PinaforeMutableSet baseedit) where
    mapTypeRange f (MkPinaforeMutableSet r s) = MkPinaforeMutableSet (mapTypeRange f r) s

newtype PinaforeConstSet baseedit t =
    MkPinaforeConstSet (PinaforeFunctionValue baseedit (FiniteSet t))

instance Functor (PinaforeConstSet baseedit) where
    fmap ab (MkPinaforeConstSet ef) = MkPinaforeConstSet $ wholeEditFunction (fmap ab) . ef

instance IsoVariant (PinaforeConstSet baseedit)

data PinaforeMorphism baseedit pqa pqb =
    forall a b. MkPinaforeMorphism (TypeRange a pqa)
                                   (TypeRange b pqb)
                                   (PinaforeLensMorphism baseedit a b)

instance IsoMapTypeRange (PinaforeMorphism baseedit pqa)

instance MapTypeRange (PinaforeMorphism baseedit pqa) where
    mapTypeRange f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (mapTypeRange f rb) m

instance IsoMapTypeRange' (PinaforeMorphism baseedit)

instance MapTypeRange' (PinaforeMorphism baseedit) where
    mapTypeRange' f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism (mapTypeRange f ra) rb m

type PinaforeRangeType baseedit = TypeRangeWitness (PinaforeType baseedit)

newtype Entity (name :: Symbol) =
    MkEntity Point

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    LimitPinaforeType :: PinaforeType baseedit polarity (LimitType polarity)
    JoinMeetPinaforeType
        :: PinaforeType baseedit polarity a
        -> PinaforeType baseedit polarity b
        -> PinaforeType baseedit polarity (JoinMeetType polarity a b)
    FunctionPinaforeType
        :: PinaforeType baseedit (InvertPolarity polarity) a
        -> PinaforeType baseedit polarity b
        -> PinaforeType baseedit polarity (Func a b)
    ListPinaforeType :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity [a]
    PairPinaforeType
        :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity b -> PinaforeType baseedit polarity (a, b)
    MutableReferencePinaforeType
        :: PinaforeRangeType baseedit polarity a -> PinaforeType baseedit polarity (PinaforeMutableReference baseedit a)
    ConstReferencePinaforeType
        :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity (PinaforeConstReference baseedit a)
    MutableSetPinaforeType
        :: PinaforeRangeType baseedit polarity a -> PinaforeType baseedit polarity (PinaforeMutableSet baseedit a)
    ConstSetPinaforeType
        :: PinaforeType baseedit polarity a -> PinaforeType baseedit polarity (PinaforeConstSet baseedit a)
    MorphismPinaforeType
        :: PinaforeRangeType baseedit polarity a
        -> PinaforeRangeType baseedit polarity b
        -> PinaforeType baseedit polarity (PinaforeMorphism baseedit a b)
    InverseMorphismPinaforeType
        :: PinaforeRangeType baseedit polarity a
        -> PinaforeRangeType baseedit polarity b
        -> PinaforeType baseedit polarity (PinaforeMorphism baseedit a b)
    GroundPinaforeType :: GroundType baseedit t -> PinaforeType baseedit polarity t
    VarPinaforeType :: SymbolWitness name -> PinaforeType baseedit polarity (UVar name)

renamePinaforeRangeTypeVars ::
       forall baseedit polarity t1 r. IsTypePolarity polarity
    => (String -> String)
    -> PinaforeRangeType baseedit polarity t1
    -> (forall t2. PinaforeRangeType baseedit polarity t2 -> WithRange Bijection t1 t2 -> r)
    -> r
renamePinaforeRangeTypeVars sf (MkTypeRangeWitness ta tb) cont =
    case isInvertPolarity @polarity of
        Dict ->
            renamePinaforeTypeVars sf ta $ \ta' bija ->
                renamePinaforeTypeVars sf tb $ \tb' bijb ->
                    cont (MkTypeRangeWitness ta' tb') $ MkWithRange (invert bija) bijb

renamePinaforeTypeVars ::
       forall baseedit polarity t1 r. IsTypePolarity polarity
    => (String -> String)
    -> PinaforeType baseedit polarity t1
    -> (forall t2. PinaforeType baseedit polarity t2 -> Bijection t1 t2 -> r)
    -> r
renamePinaforeTypeVars _ LimitPinaforeType cont = cont LimitPinaforeType id
renamePinaforeTypeVars sf (JoinMeetPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb -> cont (JoinMeetPinaforeType ta' tb') $ jmBiMap @polarity bija bijb
renamePinaforeTypeVars sf (FunctionPinaforeType ta tb) cont =
    case isInvertPolarity @polarity of
        Dict ->
            renamePinaforeTypeVars sf ta $ \ta' bija ->
                renamePinaforeTypeVars sf tb $ \tb' bijb ->
                    cont (FunctionPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars sf (ListPinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ListPinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (PairPinaforeType ta tb) cont =
    renamePinaforeTypeVars sf ta $ \ta' bija ->
        renamePinaforeTypeVars sf tb $ \tb' bijb -> cont (PairPinaforeType ta' tb') $ biIsoBi' bija . biIsoBi bijb
renamePinaforeTypeVars sf (MutableReferencePinaforeType t) cont =
    renamePinaforeRangeTypeVars sf t $ \t' bij -> cont (MutableReferencePinaforeType t') $ isoBiTypeRange bij
renamePinaforeTypeVars sf (ConstReferencePinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ConstReferencePinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (MutableSetPinaforeType t) cont =
    renamePinaforeRangeTypeVars sf t $ \t' bij -> cont (MutableSetPinaforeType t') $ isoBiTypeRange bij
renamePinaforeTypeVars sf (ConstSetPinaforeType t) cont =
    renamePinaforeTypeVars sf t $ \t' bij -> cont (ConstSetPinaforeType t') $ biIsoBi bij
renamePinaforeTypeVars sf (MorphismPinaforeType ta tb) cont =
    renamePinaforeRangeTypeVars sf ta $ \ta' bija ->
        renamePinaforeRangeTypeVars sf tb $ \tb' bijb ->
            cont (MorphismPinaforeType ta' tb') $ isoBiTypeRange' bija . isoBiTypeRange bijb
renamePinaforeTypeVars sf (InverseMorphismPinaforeType ta tb) cont =
    renamePinaforeRangeTypeVars sf ta $ \ta' bija ->
        renamePinaforeRangeTypeVars sf tb $ \tb' bijb ->
            cont (InverseMorphismPinaforeType ta' tb') $ isoBiTypeRange' bija . isoBiTypeRange bijb
renamePinaforeTypeVars _ (GroundPinaforeType t) cont = cont (GroundPinaforeType t) id
renamePinaforeTypeVars sf (VarPinaforeType namewit1) cont =
    renameUVar sf namewit1 $ \namewit2 bij -> cont (VarPinaforeType namewit2) bij

getRangeTypeVars :: PinaforeRangeType baseedit polarity t -> [String]
getRangeTypeVars (MkTypeRangeWitness tp tq) = getTypeVars tp <> getTypeVars tq

getTypeVars :: PinaforeType baseedit polarity t -> [String]
getTypeVars LimitPinaforeType = mempty
getTypeVars (JoinMeetPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (FunctionPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (ListPinaforeType t) = getTypeVars t
getTypeVars (PairPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (MutableReferencePinaforeType t) = getRangeTypeVars t
getTypeVars (ConstReferencePinaforeType t) = getTypeVars t
getTypeVars (MutableSetPinaforeType t) = getRangeTypeVars t
getTypeVars (ConstSetPinaforeType t) = getTypeVars t
getTypeVars (MorphismPinaforeType ta tb) = getRangeTypeVars ta <> getRangeTypeVars tb
getTypeVars (InverseMorphismPinaforeType ta tb) = getRangeTypeVars ta <> getRangeTypeVars tb
getTypeVars (GroundPinaforeType _) = mempty
getTypeVars (VarPinaforeType swit) = pure $ fromSymbolWitness swit

data Biunification baseedit =
    forall p q. MkBiunification (PinaforeType baseedit 'PositivePolarity p)
                                (PinaforeType baseedit 'NegativePolarity q)

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

isPinaforeRangeSubtype ::
       PinaforeRangeType baseedit 'PositivePolarity p
    -> PinaforeRangeType baseedit 'NegativePolarity q
    -> BiunifierM baseedit (WithRange (->) p q)
isPinaforeRangeSubtype (MkTypeRangeWitness tpa tpb) (MkTypeRangeWitness tqa tqb) = do
    bb <- isPinaforeSubtype tpb tqb
    aa <- isPinaforeSubtype tqa tpa
    return $ MkWithRange aa bb

isPinaforeSubtype ::
       PinaforeType baseedit 'PositivePolarity p
    -> PinaforeType baseedit 'NegativePolarity q
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
    pq <- isPinaforeRangeSubtype tp tq
    return $ mapTypeRange pq
isPinaforeSubtype (ConstReferencePinaforeType tp) (ConstReferencePinaforeType tq) = do
    pq <- isPinaforeSubtype tp tq
    return $ fmap pq
isPinaforeSubtype (MutableSetPinaforeType tp) (MutableSetPinaforeType tq) = do
    pq <- isPinaforeRangeSubtype tp tq
    return $ mapTypeRange pq
isPinaforeSubtype (ConstSetPinaforeType tp) (ConstSetPinaforeType tq) = do
    pq <- isPinaforeSubtype tp tq
    return $ fmap pq
isPinaforeSubtype (MorphismPinaforeType tpa tpb) (MorphismPinaforeType tqa tqb) = do
    aa <- isPinaforeRangeSubtype tpa tqa
    bb <- isPinaforeRangeSubtype tpb tqb
    return $ \pp -> mapTypeRange' aa $ mapTypeRange bb $ pp
isPinaforeSubtype (InverseMorphismPinaforeType tpa tpb) (InverseMorphismPinaforeType tqa tqb) = do
    aa <- isPinaforeRangeSubtype tpa tqa
    bb <- isPinaforeRangeSubtype tpb tqb
    return $ \pp -> mapTypeRange' aa $ mapTypeRange bb $ pp
isPinaforeSubtype (GroundPinaforeType tp) (GroundPinaforeType tq) = lift $ isSubtype tp tq
isPinaforeSubtype (VarPinaforeType swit) tq = do
    tellBiunification swit $ MkBiunification LimitPinaforeType tq
    return $ biBackwards unsafeUVarBijection
isPinaforeSubtype tp (VarPinaforeType swit) = do
    tellBiunification swit $ MkBiunification tp LimitPinaforeType
    return $ biForwards unsafeUVarBijection
isPinaforeSubtype tp tq = fail $ unpack $ "cannot match " <> exprShow tp <> " with " <> exprShow tq

simplifyType ::
       forall baseedit polarity a r. IsTypePolarity polarity
    => PinaforeType baseedit polarity a
    -> (forall b. PinaforeType baseedit polarity b -> Bijection a b -> r)
    -> r
simplifyType (JoinMeetPinaforeType LimitPinaforeType tb) cont = cont tb $ jmLeftIdentity @polarity
simplifyType (JoinMeetPinaforeType ta LimitPinaforeType) cont = cont ta $ jmRightIdentity @polarity
simplifyType t cont = cont t id

instance IsTypePolarity polarity => ExprShow (PinaforeType baseedit polarity t) where
    exprShowPrec LimitPinaforeType = (showLimitType @polarity, 0)
    exprShowPrec (JoinMeetPinaforeType ta tb) =
        (exprPrecShow 2 ta <> " " <> showJoinMeetType @polarity <> " " <> exprPrecShow 2 tb, 3)
    exprShowPrec (FunctionPinaforeType ta tb) =
        case isInvertPolarity @polarity of
            Dict -> (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
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

instance IsTypePolarity polarity => ExprShow (PinaforeRangeType baseedit polarity a) where
    exprShowPrec (MkTypeRangeWitness LimitPinaforeType LimitPinaforeType) = ("0", 0)
    exprShowPrec (MkTypeRangeWitness LimitPinaforeType t) = ("+" <> exprPrecShow 0 t, 0)
    exprShowPrec (MkTypeRangeWitness t LimitPinaforeType) =
        case isInvertPolarity @polarity of
            Dict -> ("-" <> exprPrecShow 0 t, 0)
    exprShowPrec (MkTypeRangeWitness t1 t2) =
        case isInvertPolarity @polarity of
            Dict ->
                case (exprShowPrec t1, exprShowPrec t2) of
                    (sp1, sp2)
                        | sp1 == sp2 -> sp1
                    (sp1, sp2) -> ("(" <> "-" <> precShow 0 sp1 <> " / " <> "+" <> precShow 0 sp2 <> ")", 0)

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

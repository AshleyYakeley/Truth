{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type
    ( module Pinafore.Language.Type
    , UVar
    ) where

import GHC.TypeLits
import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Typed
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.Order
import Pinafore.Literal
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.Table (Point)
import Pinafore.Types
import Prelude (Bounded(..))
import Shapes
import Truth.Core

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

-- could really use https://github.com/ghc-proposals/ghc-proposals/pull/81
data PinaforeGroundType baseedit (dk :: DolanVariance) (t :: DolanVarianceKind dk) where
    ActionPinaforeGroundType :: PinaforeGroundType baseedit '[] (QAction baseedit)
    OrderPinaforeGroundType :: PinaforeGroundType baseedit '[] (QOrder baseedit)
    UserInterfacePinaforeGroundType :: PinaforeGroundType baseedit '[] (UISpec (ConstEdit Point) baseedit)
    LiteralPinaforeGroundType :: LiteralType t -> PinaforeGroundType baseedit '[] t
    PointPinaforeGroundType :: SymbolWitness name -> PinaforeGroundType baseedit '[] (Entity name)
    FuncPinaforeGroundType :: PinaforeGroundType baseedit '[ 'Contravariance, 'Covariance] (->)
    ListPinaforeGroundType :: PinaforeGroundType baseedit '[ 'Covariance] []
    PairPinaforeGroundType :: PinaforeGroundType baseedit '[ 'Covariance, 'Covariance] (,)
    MutableReferencePinaforeGroundType
        :: PinaforeGroundType baseedit '[ 'Rangevariance] (PinaforeMutableReference baseedit)
    ConstReferencePinaforeGroundType :: PinaforeGroundType baseedit '[ 'Covariance] (PinaforeConstReference baseedit)
    MutableSetPinaforeGroundType :: PinaforeGroundType baseedit '[ 'Rangevariance] (PinaforeMutableSet baseedit)
    ConstSetPinaforeGroundType :: PinaforeGroundType baseedit '[ 'Covariance] (PinaforeConstSet baseedit)
    MorphismPinaforeGroundType
        :: PinaforeGroundType baseedit '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit)
    InverseMorphismPinaforeGroundType
        :: PinaforeGroundType baseedit '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit)

testPinaforeGroundTypeEquality ::
       PinaforeGroundType baseedit dka ta -> PinaforeGroundType baseedit dkb tb -> Maybe (dka :~: dkb, ta :~~: tb)
testPinaforeGroundTypeEquality ActionPinaforeGroundType ActionPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality OrderPinaforeGroundType OrderPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality UserInterfacePinaforeGroundType UserInterfacePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality (LiteralPinaforeGroundType t1) (LiteralPinaforeGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality (PointPinaforeGroundType t1) (PointPinaforeGroundType t2) = do
    Refl <- testEquality t1 t2
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality FuncPinaforeGroundType FuncPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ListPinaforeGroundType ListPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality PairPinaforeGroundType PairPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MutableReferencePinaforeGroundType MutableReferencePinaforeGroundType =
    Just (Refl, HRefl)
testPinaforeGroundTypeEquality ConstReferencePinaforeGroundType ConstReferencePinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MutableSetPinaforeGroundType MutableSetPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality ConstSetPinaforeGroundType ConstSetPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality MorphismPinaforeGroundType MorphismPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality InverseMorphismPinaforeGroundType InverseMorphismPinaforeGroundType = Just (Refl, HRefl)
testPinaforeGroundTypeEquality _ _ = Nothing

instance HasDolanVary '[ 'Rangevariance] (PinaforeMutableReference baseedit) where
    dolanVary =
        ConsDolanKindVary
            (mkRangevary $ \mapr (MkPinaforeMutableReference range lval) -> MkPinaforeMutableReference (mapr range) lval) $
        NilDolanKindVary

instance HasDolanVary '[ 'Covariance] (PinaforeConstReference baseedit) where
    dolanVary = ConsDolanKindVary fmap $ NilDolanKindVary

instance HasDolanVary '[ 'Rangevariance] (PinaforeMutableSet baseedit) where
    dolanVary =
        ConsDolanKindVary
            (mkRangevary $ \mapr (MkPinaforeMutableSet range lval) -> MkPinaforeMutableSet (mapr range) lval) $
        NilDolanKindVary

instance HasDolanVary '[ 'Covariance] (PinaforeConstSet baseedit) where
    dolanVary = ConsDolanKindVary fmap NilDolanKindVary

instance HasDolanVary '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit) where
    dolanVary =
        ConsDolanKindVary
            (mkRangevary $ \mapr ->
                 MkNestedMorphism $ \(MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism (mapr ra) rb lm) $
        ConsDolanKindVary (mkRangevary $ \mapr (MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism ra (mapr rb) lm) $
        NilDolanKindVary

pinaforeGroundTypeVary ::
       forall baseedit (dk :: DolanVariance) (f :: DolanVarianceKind dk).
       PinaforeGroundType baseedit dk f
    -> DolanKindVary dk f
pinaforeGroundTypeVary ActionPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary OrderPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary UserInterfacePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary (LiteralPinaforeGroundType _) = dolanVary @dk
pinaforeGroundTypeVary (PointPinaforeGroundType _) = dolanVary @dk
pinaforeGroundTypeVary FuncPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ListPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary PairPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MutableReferencePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ConstReferencePinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MutableSetPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary ConstSetPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary MorphismPinaforeGroundType = dolanVary @dk
pinaforeGroundTypeVary InverseMorphismPinaforeGroundType = dolanVary @dk

pinaforeGroundTypeKind :: PinaforeGroundType baseedit dk t -> DolanVarianceType dk
pinaforeGroundTypeKind ActionPinaforeGroundType = representative
pinaforeGroundTypeKind OrderPinaforeGroundType = representative
pinaforeGroundTypeKind UserInterfacePinaforeGroundType = representative
pinaforeGroundTypeKind (LiteralPinaforeGroundType _) = representative
pinaforeGroundTypeKind (PointPinaforeGroundType _) = representative
pinaforeGroundTypeKind FuncPinaforeGroundType = representative
pinaforeGroundTypeKind ListPinaforeGroundType = representative
pinaforeGroundTypeKind PairPinaforeGroundType = representative
pinaforeGroundTypeKind MutableReferencePinaforeGroundType = representative
pinaforeGroundTypeKind ConstReferencePinaforeGroundType = representative
pinaforeGroundTypeKind MutableSetPinaforeGroundType = representative
pinaforeGroundTypeKind ConstSetPinaforeGroundType = representative
pinaforeGroundTypeKind MorphismPinaforeGroundType = representative
pinaforeGroundTypeKind InverseMorphismPinaforeGroundType = representative

data PinaforeType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    NilPinaforeType :: PinaforeType baseedit polarity (LimitType polarity)
    ConsPinaforeType
        :: PinaforeSingularType baseedit polarity t
        -> PinaforeType baseedit polarity tr
        -> PinaforeType baseedit polarity (JoinMeetType polarity t tr)

type PinaforeTypeF (baseedit :: Type) = TypeF (PinaforeType baseedit)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeSingularType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    GroundPinaforeSingularType
        :: PinaforeGroundType baseedit dv t
        -> DolanArguments dv (PinaforeType baseedit) t polarity ta
        -> PinaforeSingularType baseedit polarity ta
    VarPinaforeSingularType :: SymbolWitness name -> PinaforeSingularType baseedit polarity (UVar name)

singlePositivePinaforeTypeF ::
       TypeF (PinaforeSingularType baseedit) 'PositivePolarity t -> PinaforeTypeF baseedit 'PositivePolarity t
singlePositivePinaforeTypeF (MkTypeF st conv) = contramap conv $ MkTypeF (ConsPinaforeType st NilPinaforeType) join1

singleNegativePinaforeTypeF ::
       TypeF (PinaforeSingularType baseedit) 'NegativePolarity t -> PinaforeTypeF baseedit 'NegativePolarity t
singleNegativePinaforeTypeF (MkTypeF st conv) = fmap conv $ MkTypeF (ConsPinaforeType st NilPinaforeType) meet1

type LiftBijection (f :: kp -> kq) = forall (a :: kp) (b :: kp). KindBijection kp a b -> KindBijection kq (f a) (f b)

vcBijection ::
       forall (v :: SingleVariance) k (f :: SingleVarianceKind v -> k). HasKindMorphism k
    => SingleVarianceType v
    -> SingleVarianceMap v f
    -> LiftBijection f
vcBijection CovarianceType conv (MkBijection ab ba) = mkKindBijection (conv ab) (conv ba)
vcBijection ContravarianceType conv (MkBijection ba ab) = mkKindBijection (conv $ MkCatDual ab) (conv $ MkCatDual ba)
vcBijection RangevarianceType conv (MkPairMorphism (MkBijection papb pbpa) (MkBijection qaqb qbqa)) =
    mkKindBijection (conv $ MkRangeFunc pbpa qaqb) (conv $ MkRangeFunc papb qbqa)

getRangeTypeVars :: PinaforeRangeType baseedit polarity t -> [String]
getRangeTypeVars (MkTypeRangeWitness tp tq) = getTypeVars tp <> getTypeVars tq

getArgTypeVars ::
       forall baseedit polarity v a.
       SingleVarianceType v
    -> SingleArgument v (PinaforeType baseedit) polarity a
    -> [String]
getArgTypeVars CovarianceType ft = getTypeVars ft
getArgTypeVars ContravarianceType ft = getTypeVars ft
getArgTypeVars RangevarianceType (MkTypeRangeWitness ta tb) = getTypeVars ta <> getTypeVars tb

getArgsTypeVars ::
       forall baseedit polarity dv t ta.
       DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) t polarity ta
    -> [String]
getArgsTypeVars NilListType NilDolanArguments = []
getArgsTypeVars (ConsListType tv targs) (ConsDolanArguments arg args) =
    getArgTypeVars @baseedit @polarity tv arg <> getArgsTypeVars targs args

getTypeVars :: PinaforeType baseedit polarity t -> [String]
getTypeVars NilPinaforeType = mempty
getTypeVars (ConsPinaforeType (GroundPinaforeSingularType gt args) tr) =
    getArgsTypeVars (pinaforeGroundTypeKind gt) args <> getTypeVars tr
getTypeVars (ConsPinaforeType (VarPinaforeSingularType swit) tr) = fromSymbolWitness swit : getTypeVars tr

data BisubstitutionWitness baseedit t where
    PositiveBisubstitutionWitness
        :: SymbolWitness name
        -> PinaforeSingularType baseedit 'PositivePolarity p
        -> BisubstitutionWitness baseedit (p -> UVar name)
    NegativeBisubstitutionWitness
        :: SymbolWitness name
        -> PinaforeSingularType baseedit 'NegativePolarity q
        -> BisubstitutionWitness baseedit (UVar name -> q)

type PinaforeUnifier baseedit = Expression (BisubstitutionWitness baseedit)

type PinaforeFullUnifier baseedit = Compose (Result Text) (Expression (BisubstitutionWitness baseedit))

joinPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'PositivePolarity b
    -> (forall ab. PinaforeType baseedit 'PositivePolarity ab -> (a -> ab) -> (b -> ab) -> r)
    -> r
joinPinaforeTypes NilPinaforeType tb cont = cont tb never id
joinPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    joinPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (joinBimap id conva) (join2 . convb)

joinPinaforeTypeF ::
       forall baseedit (a :: Type) (b :: Type).
       PinaforeTypeF baseedit 'PositivePolarity a
    -> PinaforeTypeF baseedit 'PositivePolarity b
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType a b)
joinPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) =
    contramap (joinBimap conva convb) $
    joinPinaforeTypes ta tb $ \tab conva' convb' -> MkTypeF tab $ joinf conva' convb'

meetPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'NegativePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> (forall ab. PinaforeType baseedit 'NegativePolarity ab -> (ab -> a) -> (ab -> b) -> r)
    -> r
meetPinaforeTypes NilPinaforeType tb cont = cont tb alwaysTop id
meetPinaforeTypes (ConsPinaforeType ta tr) tb cont =
    meetPinaforeTypes tr tb $ \trb conva convb -> cont (ConsPinaforeType ta trb) (meetBimap id conva) (convb . meet2)

meetPinaforeTypeF ::
       forall baseedit (a :: Type) (b :: Type).
       PinaforeTypeF baseedit 'NegativePolarity a
    -> PinaforeTypeF baseedit 'NegativePolarity b
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType a b)
meetPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) =
    fmap (meetBimap conva convb) $ meetPinaforeTypes ta tb $ \tab conva' convb' -> MkTypeF tab $ meetf conva' convb'

unifyPosNegVariance ::
       SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a
    -> SingleArgument sv (PinaforeType baseedit) 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (SingleVarianceFunc sv a b)
unifyPosNegVariance CovarianceType ta tb = unifyPosNegPinaforeTypes ta tb
unifyPosNegVariance ContravarianceType ta tb = do
    ba <- unifyPosNegPinaforeTypes tb ta
    return $ MkCatDual ba
unifyPosNegVariance RangevarianceType (MkTypeRangeWitness tpa tqa) (MkTypeRangeWitness tpb tqb) = do
    pba <- unifyPosNegPinaforeTypes tpb tpa
    qab <- unifyPosNegPinaforeTypes tqa tqb
    return $ MkRangeFunc pba qab

unifyPosNegArguments ::
       forall baseedit dv gta gtb ta tb.
       DolanVarianceType dv
    -> DolanKindVary dv gta
    -> DolanArguments dv (PinaforeType baseedit) gta 'PositivePolarity ta
    -> DolanArguments dv (PinaforeType baseedit) gtb 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (KindFunction (DolanVarianceKind dv) gta gtb -> ta -> tb)
unifyPosNegArguments NilListType NilDolanKindVary NilDolanArguments NilDolanArguments = pure id
unifyPosNegArguments (ConsListType (svt :: SingleVarianceType sv) (dvt :: DolanVarianceType dv')) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) = do
    sfunc <- unifyPosNegVariance svt sta stb
    f <- unifyPosNegArguments dvt dvm dta dtb
    pure $
        case dolanVarianceKMCategory @(->) dvt of
            Dict -> \(MkNestedMorphism conv) -> f (conv . svm sfunc)

unifyPosNegGroundTypes ::
       PinaforeGroundType baseedit dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta 'PositivePolarity ta
    -> PinaforeGroundType baseedit dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (ta -> tb)
unifyPosNegGroundTypes ga argsa gb argsb
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality ga gb = let
        vkt = pinaforeGroundTypeKind ga
        in case dolanVarianceKMCategory @(->) vkt of
               Dict -> do
                   f <- unifyPosNegArguments vkt (pinaforeGroundTypeVary ga) argsa argsb
                   return $ f id
unifyPosNegGroundTypes ga argsa gb argsb =
    unifierFail $
    "can't cast " <>
    (unpack $ exprShow $ GroundPinaforeSingularType ga argsa) <>
    " to " <> (unpack $ exprShow $ GroundPinaforeSingularType gb argsb)

unifyPosNegPinaforeSingularTypes ::
       PinaforeSingularType baseedit 'PositivePolarity a
    -> PinaforeSingularType baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) (VarPinaforeSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) tb =
    liftUnifier $ varExpression $ NegativeBisubstitutionWitness na tb
unifyPosNegPinaforeSingularTypes ta (VarPinaforeSingularType nb) =
    liftUnifier $ varExpression $ PositiveBisubstitutionWitness nb ta
unifyPosNegPinaforeSingularTypes (GroundPinaforeSingularType gta argsa) (GroundPinaforeSingularType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

unifyPosNegPinaforeTypes1 ::
       PinaforeSingularType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes1 _ NilPinaforeType = pure alwaysTop
unifyPosNegPinaforeTypes1 ta (ConsPinaforeType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes NilPinaforeType _ = pure never
unifyPosNegPinaforeTypes (ConsPinaforeType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeTypeF ::
       PinaforeTypeF baseedit 'PositivePolarity a
    -> PinaforeTypeF baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) = do
    conv <- unifyPosNegPinaforeTypes ta tb
    return $ convb . conv . conva

occursInArg ::
       forall baseedit polarity n sv a.
       SingleVarianceType sv
    -> SymbolWitness n
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkTypeRangeWitness tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall baseedit polarity n dv t a.
       DolanVarianceType dv
    -> SymbolWitness n
    -> DolanArguments dv (PinaforeType baseedit) t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @baseedit @polarity svt n arg || occursInArgs dvt n args

occursInSingularType :: SymbolWitness n -> PinaforeSingularType baseedit polarity a -> Bool
occursInSingularType n (VarPinaforeSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarPinaforeSingularType _) = False
occursInSingularType n (GroundPinaforeSingularType gt args) = occursInArgs (pinaforeGroundTypeKind gt) n args

occursInType :: SymbolWitness n -> PinaforeType baseedit polarity a -> Bool
occursInType _ NilPinaforeType = False
occursInType n (ConsPinaforeType t1 t2) = occursInSingularType n t1 || occursInType n t2

data Bisubstitution (wit :: TypePolarity -> Type -> Type) =
    forall name. MkBisubstitution (SymbolWitness name)
                                  (TypeF wit 'PositivePolarity (UVar name))
                                  (TypeF wit 'NegativePolarity (UVar name))

type PinaforeBisubstitution baseedit = Bisubstitution (PinaforeType baseedit)

bisubstitutePositiveSingularType ::
       PinaforeBisubstitution baseedit
    -> PinaforeSingularType baseedit 'PositivePolarity t
    -> PinaforeTypeF baseedit 'PositivePolarity t
bisubstitutePositiveSingularType (MkBisubstitution n tp _) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tp
bisubstitutePositiveSingularType _ t@(VarPinaforeSingularType _) = singlePositivePinaforeTypeF $ mkTypeF t
bisubstitutePositiveSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in case mapDolanArguments (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args of
           MkTypeF args' conv -> singlePositivePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

bisubstituteNegativeSingularType ::
       PinaforeBisubstitution baseedit
    -> PinaforeSingularType baseedit 'NegativePolarity t
    -> PinaforeTypeF baseedit 'NegativePolarity t
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarPinaforeSingularType _) = singleNegativePinaforeTypeF $ mkTypeF t
bisubstituteNegativeSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in case mapDolanArguments (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args of
           MkTypeF args' conv -> singleNegativePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

bisubstitutePositiveType ::
       PinaforeBisubstitution baseedit
    -> PinaforeType baseedit 'PositivePolarity t
    -> PinaforeTypeF baseedit 'PositivePolarity t
bisubstitutePositiveType _ NilPinaforeType = mkTypeF NilPinaforeType
bisubstitutePositiveType bisub (ConsPinaforeType ta tb) = let
    tfa = bisubstitutePositiveSingularType bisub ta
    tfb = bisubstitutePositiveType bisub tb
    in joinPinaforeTypeF tfa tfb

bisubstituteNegativeType ::
       PinaforeBisubstitution baseedit
    -> PinaforeType baseedit 'NegativePolarity t
    -> PinaforeTypeF baseedit 'NegativePolarity t
bisubstituteNegativeType _ NilPinaforeType = mkTypeF NilPinaforeType
bisubstituteNegativeType bisub (ConsPinaforeType ta tb) = let
    tfa = bisubstituteNegativeSingularType bisub ta
    tfb = bisubstituteNegativeType bisub tb
    in meetPinaforeTypeF tfa tfb

bisubstituteType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeBisubstitution baseedit
    -> PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
bisubstituteType =
    case whichTypePolarity @polarity of
        Left Refl -> bisubstitutePositiveType
        Right Refl -> bisubstituteNegativeType

bisubstitutePositiveVar ::
       SymbolWitness name
    -> PinaforeType baseedit 'PositivePolarity t
    -> PinaforeUnifier baseedit ((t -> UVar name) -> a)
    -> PinaforeUnifier baseedit a
bisubstitutePositiveVar _ NilPinaforeType uf = fmap (\fa -> fa never) uf
bisubstitutePositiveVar vn (ConsPinaforeType t1 tr) uf =
    OpenExpression (PositiveBisubstitutionWitness vn t1) $
    bisubstitutePositiveVar vn tr $ fmap (\fa fr f1 -> fa $ joinf f1 fr) uf

bisubstituteNegativeVar ::
       SymbolWitness name
    -> PinaforeType baseedit 'NegativePolarity t
    -> PinaforeUnifier baseedit ((UVar name -> t) -> a)
    -> PinaforeUnifier baseedit a
bisubstituteNegativeVar _ NilPinaforeType uf = fmap (\fa -> fa alwaysTop) uf
bisubstituteNegativeVar vn (ConsPinaforeType t1 tr) uf =
    OpenExpression (NegativeBisubstitutionWitness vn t1) $
    bisubstituteNegativeVar vn tr $ fmap (\fa fr f1 -> fa $ meetf f1 fr) uf

bisubstituteUnifier :: PinaforeBisubstitution baseedit -> PinaforeUnifier baseedit a -> PinaforeFullUnifier baseedit a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ tq) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyPosNegPinaforeTypeF (singlePositivePinaforeTypeF $ mkTypeF tp) tq
        val' <- bisubstituteUnifier bisub uval
        return $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn tp _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyPosNegPinaforeTypeF tp (singleNegativePinaforeTypeF $ mkTypeF tq)
        val' <- bisubstituteUnifier bisub uval
        return $ val' conv
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) =
    case bisubstitutePositiveSingularType bisub tp of
        MkTypeF tp' conv ->
            Compose $ do
                uval' <- getCompose $ bisubstituteUnifier bisub uval
                return $ bisubstitutePositiveVar vn tp' $ fmap (\ca pv -> ca $ (pv . conv)) uval'
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) =
    case bisubstituteNegativeSingularType bisub tp of
        MkTypeF tp' conv ->
            Compose $ do
                uval' <- getCompose $ bisubstituteUnifier bisub uval
                return $ bisubstituteNegativeVar vn tp' $ fmap (\ca pv -> ca $ (conv . pv)) uval'

runUnifier :: forall baseedit a. PinaforeUnifier baseedit a -> Result Text (a, [PinaforeBisubstitution baseedit])
runUnifier (ClosedExpression a) = return (a, [])
runUnifier (OpenExpression (PositiveBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier (OpenExpression (NegativeBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolWitness name) (tp :: PinaforeSingularType baseedit 'PositivePolarity vw)) expr) = let
    varBij :: Bijection (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (contramap (biBackwards varBij) $
             joinPinaforeTypeF
                 (singlePositivePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
                 (singlePositivePinaforeTypeF $ mkTypeF tp))
            (fmap (biForwards varBij . join1) $ singleNegativePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ biForwards varBij . join2, bisub : subs)
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolWitness name) (tq :: PinaforeSingularType baseedit 'NegativePolarity vw)) expr) = let
    varBij :: Bijection (MeetType (UVar name) vw) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (contramap (meet1 . biBackwards varBij) $ singlePositivePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
            (fmap (biForwards varBij) $
             meetPinaforeTypeF
                 (singleNegativePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
                 (singleNegativePinaforeTypeF $ mkTypeF tq))
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ meet2 . biBackwards varBij, bisub : subs)

bisubstituteAllPositiveType ::
       [PinaforeBisubstitution baseedit]
    -> PinaforeType baseedit 'PositivePolarity t
    -> PinaforeTypeF baseedit 'PositivePolarity t
bisubstituteAllPositiveType [] t = mkTypeF t
bisubstituteAllPositiveType (sub:subs) t =
    case bisubstitutePositiveType sub t of
        MkTypeF t' conv -> contramap conv $ bisubstituteAllPositiveType subs t'

bisubstituteAllNegativeType ::
       [PinaforeBisubstitution baseedit]
    -> PinaforeType baseedit 'NegativePolarity t
    -> PinaforeTypeF baseedit 'NegativePolarity t
bisubstituteAllNegativeType [] t = mkTypeF t
bisubstituteAllNegativeType (sub:subs) t =
    case bisubstituteNegativeType sub t of
        MkTypeF t' conv -> fmap conv $ bisubstituteAllNegativeType subs t'

instance Unifier (PinaforeUnifier baseedit) where
    type UnifierMonad (PinaforeUnifier baseedit) = Result Text
    type UnifierNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'NegativePolarity
    type UnifierPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'PositivePolarity
    type UnifierSubstitutions (PinaforeUnifier baseedit) = [PinaforeBisubstitution baseedit]
    unifyNegWitnesses ta tb cont = meetPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosWitnesses ta tb cont = joinPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosNegWitnesses = unifyPosNegPinaforeTypes
    solveUnifier = runUnifier
    unifierPosSubstitute subs t = unTypeF $ bisubstituteAllPositiveType subs t
    unifierNegSubstitute subs t = unTypeF $ bisubstituteAllNegativeType subs t
    -- Simplification:
    -- 1. merge duplicate ground types in join/meet (on each type)
    -- 2. eliminate one-sided vars (on whole expression)
    -- 3. merge shared vars (on whole expression)
    -- 4. merge duplicate vars in join/meet (on each type)
    simplifyExpressionType =
        mergeDuplicateVarsExpression . mergeSharedVars . eliminateOneSidedVars . mergeDuplicatesInExpression

type PinaforeTypeNamespace baseedit (w :: k -> Type)
     = forall t1 r.
               w t1 -> (forall t2.
                            InKind t2 => w t2 -> KindBijection k t1 t2 -> VarNamespace (PinaforeTypeSystem baseedit) r) -> VarNamespace (PinaforeTypeSystem baseedit) r

renamePinaforeRangeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeRangeType baseedit polarity)
renamePinaforeRangeTypeVars (MkTypeRangeWitness ta tb) cont =
    case isInvertPolarity @polarity of
        Dict ->
            renamePinaforeTypeVars ta $ \ta' bija ->
                renamePinaforeTypeVars tb $ \tb' bijb -> cont (MkTypeRangeWitness ta' tb') $ MkPairMorphism bija bijb

renameTypeArg ::
       forall baseedit polarity v. IsTypePolarity polarity
    => SingleVarianceType v
    -> PinaforeTypeNamespace baseedit (SingleArgument v (PinaforeType baseedit) polarity)
renameTypeArg CovarianceType = renamePinaforeTypeVars
renameTypeArg ContravarianceType =
    case isInvertPolarity @polarity of
        Dict -> renamePinaforeTypeVars
renameTypeArg RangevarianceType = renamePinaforeRangeTypeVars

renameTypeArgs ::
       forall baseedit (polarity :: TypePolarity) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanKindVary dv t
    -> PinaforeTypeNamespace baseedit (DolanArguments dv (PinaforeType baseedit) t polarity)
renameTypeArgs NilListType NilDolanKindVary NilDolanArguments cont = cont NilDolanArguments id
renameTypeArgs (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments arg args) cont =
    renameTypeArg @baseedit @polarity svt arg $ \arg' bijarg ->
        case dolanVarianceHasKM dvt of
            Dict ->
                bijectTypeArguments (vcBijection svt svm bijarg) args $ \args' bijargs ->
                    renameTypeArgs dvt dvm args' $ \args'' bijargs' ->
                        cont (ConsDolanArguments arg' args'') $ bijargs' . bijargs

renamePinaforeSinglularTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeSingularType baseedit polarity)
renamePinaforeSinglularTypeVars (GroundPinaforeSingularType gt args) cont =
    renameTypeArgs @baseedit @polarity (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args $ \args' bij ->
        cont (GroundPinaforeSingularType gt args') bij
renamePinaforeSinglularTypeVars (VarPinaforeSingularType namewit1) cont =
    renameUVar varNamespaceRename namewit1 $ \namewit2 bij -> cont (VarPinaforeSingularType namewit2) bij

renamePinaforeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeType baseedit polarity)
renamePinaforeTypeVars NilPinaforeType cont = cont NilPinaforeType id
renamePinaforeTypeVars (ConsPinaforeType ta tb) cont =
    renamePinaforeSinglularTypeVars ta $ \ta' bija ->
        renamePinaforeTypeVars tb $ \tb' bijb -> cont (ConsPinaforeType ta' tb') $ jmBiMap @polarity bija bijb

data PinaforeTypeSystem (baseedit :: Type)

instance Namespace (VarNamespace (PinaforeTypeSystem baseedit)) where
    type NamespaceNegWitness (VarNamespace (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'NegativePolarity
    type NamespacePosWitness (VarNamespace (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'PositivePolarity
    renameNegWitness = renamePinaforeTypeVars
    renamePosWitness = renamePinaforeTypeVars

instance Renamer (VarRenamer (PinaforeTypeSystem baseedit)) where
    type RenamerNamespace (VarRenamer (PinaforeTypeSystem baseedit)) = VarNamespace (PinaforeTypeSystem baseedit)
    renameNewVar cont = do
        n <- varRenamerGenerate
        toSymbolWitness n $ \wit ->
            cont
                (ConsPinaforeType (VarPinaforeSingularType wit) NilPinaforeType)
                (ConsPinaforeType (VarPinaforeSingularType wit) NilPinaforeType) $
            join1 . meet1
    namespace = runVarNamespace
    runRenamer = runVarRenamer

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TypeRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TypeUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type NegWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'NegativePolarity
    type PosWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'PositivePolarity
    type TSMonad (PinaforeTypeSystem baseedit) = Result Text
    typeSystemFunctionPosWitness ta tb =
        unTypeF $
        singlePositivePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    typeSystemFunctionNegWitness ta tb =
        unTypeF $
        singleNegativePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance IsTypePolarity polarity => ExprShow (PinaforeSingularType baseedit polarity t) where
    exprShowPrec (VarPinaforeSingularType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundPinaforeSingularType gt args) = exprShowPrecGroundType gt args

instance IsTypePolarity polarity => ExprShow (PinaforeType baseedit polarity t) where
    exprShowPrec NilPinaforeType = (showLimitType @polarity, 0)
    exprShowPrec (ConsPinaforeType ta NilPinaforeType) = exprShowPrec ta
    exprShowPrec (ConsPinaforeType ta tb) =
        (exprPrecShow 2 ta <> " " <> showJoinMeetType @polarity <> " " <> exprPrecShow 2 tb, 3)

instance IsTypePolarity polarity => Show (PinaforeType baseedit polarity t) where
    show v = unpack $ exprShow v

exprShowPrecGroundType ::
       forall baseedit polarity dv t ta. IsTypePolarity polarity
    => PinaforeGroundType baseedit dv t
    -> DolanArguments dv (PinaforeType baseedit) t polarity ta
    -> (Text, Int)
exprShowPrecGroundType ActionPinaforeGroundType NilDolanArguments = ("Action", 0)
exprShowPrecGroundType OrderPinaforeGroundType NilDolanArguments = ("Order", 0)
exprShowPrecGroundType UserInterfacePinaforeGroundType NilDolanArguments = ("UI", 0)
exprShowPrecGroundType (LiteralPinaforeGroundType t) NilDolanArguments = exprShowPrec t
exprShowPrecGroundType (PointPinaforeGroundType n) NilDolanArguments = (pack $ show n, 0)
exprShowPrecGroundType FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    case isInvertPolarity @polarity of
        Dict -> (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
exprShowPrecGroundType ListPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) = ("[" <> exprShow ta <> "]", 0)
exprShowPrecGroundType PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    ("(" <> exprShow ta <> ", " <> exprShow tb <> ")", 0)
exprShowPrecGroundType MutableReferencePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("MRef " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType ConstReferencePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("CRef " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType MutableSetPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("MSet " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType ConstSetPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("CSet " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    case isInvertPolarity @polarity of
        Dict -> (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)
exprShowPrecGroundType InverseMorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    case isInvertPolarity @polarity of
        Dict -> (exprPrecShow 2 ta <> " <~ " <> exprPrecShow 3 tb, 3)

instance IsTypePolarity polarity => ExprShow (PinaforeRangeType baseedit polarity a) where
    exprShowPrec (MkTypeRangeWitness NilPinaforeType NilPinaforeType) = ("0", 0)
    exprShowPrec (MkTypeRangeWitness NilPinaforeType t) = ("+" <> exprPrecShow 0 t, 0)
    exprShowPrec (MkTypeRangeWitness t NilPinaforeType) =
        case isInvertPolarity @polarity of
            Dict -> ("-" <> exprPrecShow 0 t, 0)
    exprShowPrec (MkTypeRangeWitness t1 t2) =
        case isInvertPolarity @polarity of
            Dict ->
                case (exprShowPrec t1, exprShowPrec t2) of
                    (sp1, sp2)
                        | sp1 == sp2 -> sp1
                    (sp1, sp2) -> ("(" <> "-" <> precShow 0 sp1 <> " / " <> "+" <> precShow 0 sp2 <> ")", 0)

class IsSubtype w where
    isSubtype :: w a -> w b -> Result Text (a -> b)

data LiteralType (t :: Type) where
    LiteralLiteralType :: LiteralType Literal
    TextLiteralType :: LiteralType Text
    NumberLiteralType :: LiteralType Number
    BooleanLiteralType :: LiteralType Bool
    BottomLiteralType :: LiteralType BottomType

instance TestEquality LiteralType where
    testEquality LiteralLiteralType LiteralLiteralType = Just Refl
    testEquality TextLiteralType TextLiteralType = Just Refl
    testEquality NumberLiteralType NumberLiteralType = Just Refl
    testEquality BooleanLiteralType BooleanLiteralType = Just Refl
    testEquality BottomLiteralType BottomLiteralType = Just Refl
    testEquality _ _ = Nothing

instance ExprShow (LiteralType t) where
    exprShowPrec LiteralLiteralType = ("Literal", 0)
    exprShowPrec TextLiteralType = ("Text", 0)
    exprShowPrec NumberLiteralType = ("Number", 0)
    exprShowPrec BooleanLiteralType = ("Boolean", 0)
    exprShowPrec BottomLiteralType = ("LiteralBottom", 0)

instance IsSubtype LiteralType where
    isSubtype LiteralLiteralType LiteralLiteralType = return id
    isSubtype TextLiteralType LiteralLiteralType = return toLiteral
    isSubtype NumberLiteralType LiteralLiteralType = return toLiteral
    isSubtype BooleanLiteralType LiteralLiteralType = return toLiteral
    isSubtype TextLiteralType TextLiteralType = return id
    isSubtype NumberLiteralType NumberLiteralType = return id
    isSubtype BooleanLiteralType BooleanLiteralType = return id
    isSubtype BottomLiteralType _ = return never
    isSubtype ta tb = FailureResult $ "cannot match " <> exprShow ta <> " with " <> exprShow tb

type PinaforeExpression baseedit name
     = SealedExpression name (PinaforeType baseedit 'NegativePolarity) (PinaforeType baseedit 'PositivePolarity)

mergeJoin :: JoinType a (JoinType a b) -> JoinType a b
mergeJoin (LeftJoinType v) = LeftJoinType v
mergeJoin (RightJoinType v) = v

mergeMeet :: MeetType a b -> MeetType a (MeetType a b)
mergeMeet (MkMeetType (a, b)) = MkMeetType (a, MkMeetType (a, b))

swapJoinRight :: JoinType a (JoinType b c) -> JoinType (JoinType a b) c
swapJoinRight (LeftJoinType v) = LeftJoinType $ LeftJoinType v
swapJoinRight (RightJoinType (LeftJoinType v)) = LeftJoinType $ RightJoinType v
swapJoinRight (RightJoinType (RightJoinType v)) = RightJoinType v

swapMeetRight :: MeetType (MeetType a b) c -> MeetType a (MeetType b c)
swapMeetRight (MkMeetType (MkMeetType (a, b), c)) = MkMeetType (a, MkMeetType (b, c))

consPositiveDolanArguments ::
       forall ft sv dv t (a :: SingleVarianceKind sv) ta. InKind a
    => SingleArgument sv ft 'PositivePolarity a
    -> TypeF (DolanArguments dv ft (t a)) 'PositivePolarity ta
    -> TypeF (DolanArguments (sv ': dv) ft t) 'PositivePolarity ta
consPositiveDolanArguments svt (MkTypeF dvt conv) = MkTypeF (ConsDolanArguments svt dvt) conv

mergeDuplicatesInSingularType ::
       IsTypePolarity polarity
    => PinaforeSingularType baseedit polarity t
    -> TypeF (PinaforeSingularType baseedit) polarity t
mergeDuplicatesInSingularType (GroundPinaforeSingularType gt args) =
    case mapDolanArguments mergeDuplicatesInType (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args of
        MkTypeF args' conv -> MkTypeF (GroundPinaforeSingularType gt args') conv
mergeDuplicatesInSingularType t = mkTypeF t

mergeDuplicatePositiveSingularGroundTypes ::
       PinaforeSingularType baseedit 'PositivePolarity t1
    -> PinaforeType baseedit 'PositivePolarity tr
    -> PinaforeTypeF baseedit 'PositivePolarity (JoinType t1 tr)
mergeDuplicatePositiveSingularGroundTypes ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeDuplicatePositiveSingularGroundTypes (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        contramap mergeJoin $ mergeDuplicatePositiveSingularGroundTypes (VarPinaforeSingularType vn1) tr
mergeDuplicatePositiveSingularGroundTypes (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeDuplicatesInTypes
                 (pinaforeGroundTypeKind gt1)
                 (pinaforeGroundTypeVary gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                contramap (joinBimap convargs id . swapJoinRight) $
                mergeDuplicatePositiveSingularGroundTypes (GroundPinaforeSingularType gt1 args') tr
mergeDuplicatePositiveSingularGroundTypes ts (ConsPinaforeType t1 tr) =
    case mergeDuplicatePositiveSingularGroundTypes ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \case
                LeftJoinType v -> RightJoinType $ conv $ LeftJoinType v
                RightJoinType (LeftJoinType v) -> LeftJoinType v
                RightJoinType (RightJoinType v) -> RightJoinType $ conv $ RightJoinType v

mergeDuplicateNegativeSingularGroundTypes ::
       PinaforeSingularType baseedit 'NegativePolarity t1
    -> PinaforeType baseedit 'NegativePolarity tr
    -> PinaforeTypeF baseedit 'NegativePolarity (MeetType t1 tr)
mergeDuplicateNegativeSingularGroundTypes ts NilPinaforeType = mkTypeF $ ConsPinaforeType ts NilPinaforeType
mergeDuplicateNegativeSingularGroundTypes (VarPinaforeSingularType vn1) (ConsPinaforeType (VarPinaforeSingularType vn2) tr)
    | Just Refl <- testEquality vn1 vn2 =
        fmap mergeMeet $ mergeDuplicateNegativeSingularGroundTypes (VarPinaforeSingularType vn1) tr
mergeDuplicateNegativeSingularGroundTypes (GroundPinaforeSingularType gt1 args1) (ConsPinaforeType (GroundPinaforeSingularType gt2 args2) tr)
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality gt1 gt2 =
        case mergeDolanArguments
                 mergeDuplicatesInTypes
                 (pinaforeGroundTypeKind gt1)
                 (pinaforeGroundTypeVary gt1)
                 args1
                 args2 of
            MkTypeF args' convargs ->
                fmap (swapMeetRight . meetBimap convargs id) $
                mergeDuplicateNegativeSingularGroundTypes (GroundPinaforeSingularType gt1 args') tr
mergeDuplicateNegativeSingularGroundTypes ts (ConsPinaforeType t1 tr) =
    case mergeDuplicateNegativeSingularGroundTypes ts tr of
        MkTypeF tsr conv ->
            MkTypeF (ConsPinaforeType t1 tsr) $ \(MkMeetType (a, b)) ->
                MkMeetType (meet1 $ conv b, MkMeetType (a, meet2 $ conv b))

mergeDuplicatesInType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
mergeDuplicatesInType NilPinaforeType = mkTypeF NilPinaforeType
mergeDuplicatesInType (ConsPinaforeType t1 tr) =
    case mergeDuplicatesInSingularType t1 of
        MkTypeF t1' conv1 ->
            case mergeDuplicatesInType tr of
                MkTypeF tr' convr ->
                    case whichTypePolarity @polarity of
                        Left Refl ->
                            contramap (joinBimap conv1 convr) $ mergeDuplicatePositiveSingularGroundTypes t1' tr'
                        Right Refl -> fmap (meetBimap conv1 convr) $ mergeDuplicateNegativeSingularGroundTypes t1' tr'

mergeDuplicatesInTypes ::
       forall baseedit polarity ta tb. IsTypePolarity polarity
    => PinaforeType baseedit polarity ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeTypeF baseedit polarity (JoinMeetType polarity ta tb)
mergeDuplicatesInTypes ta tb =
    case whichTypePolarity @polarity of
        Left Refl -> chainTypeF mergeDuplicatesInType $ joinPinaforeTypeF (mkTypeF ta) (mkTypeF tb)
        Right Refl -> chainTypeF mergeDuplicatesInType $ meetPinaforeTypeF (mkTypeF ta) (mkTypeF tb)

mergeDuplicatesInExpression :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicatesInExpression = mapSealedExpressionTypes mergeDuplicatesInType mergeDuplicatesInType

class GetExpressionVars t where
    -- | (positive, negative)
    getExpressionVars :: t -> ([AnyWitness SymbolWitness], [AnyWitness SymbolWitness])

instance IsTypePolarity polarity => GetExpressionVars (TypeRangeWitness (PinaforeType baseedit) polarity a) where
    getExpressionVars (MkTypeRangeWitness tp tq) =
        case isInvertPolarity @polarity of
            Dict -> getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([AnyWitness SymbolWitness], [AnyWitness SymbolWitness])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t =
    case isInvertPolarity @polarity of
        Dict -> getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([AnyWitness SymbolWitness], [AnyWitness SymbolWitness])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @baseedit @polarity sv arg <> getArgsExpressionVars dv args

instance IsTypePolarity polarity => GetExpressionVars (PinaforeSingularType baseedit polarity t) where
    getExpressionVars (GroundPinaforeSingularType gt args) = getArgsExpressionVars (pinaforeGroundTypeKind gt) args
    getExpressionVars (VarPinaforeSingularType vn) =
        case whichTypePolarity @polarity of
            Left Refl -> ([MkAnyWitness vn], [])
            Right Refl -> ([], [MkAnyWitness vn])

instance IsTypePolarity polarity => GetExpressionVars (PinaforeType baseedit polarity t) where
    getExpressionVars NilPinaforeType = mempty
    getExpressionVars (ConsPinaforeType t1 tr) = getExpressionVars t1 <> getExpressionVars tr

instance GetExpressionVars (NamedExpression name (PinaforeType baseedit 'NegativePolarity) t) where
    getExpressionVars (ClosedExpression _) = mempty
    getExpressionVars (OpenExpression (MkNameWitness _ t) expr) = getExpressionVars t <> getExpressionVars expr

instance GetExpressionVars (PinaforeExpression baseedit name) where
    getExpressionVars (MkSealedExpression twt expr) = getExpressionVars twt <> getExpressionVars expr

bisubstitutesSealedExpression ::
       [PinaforeBisubstitution baseedit] -> PinaforeExpression baseedit name -> PinaforeExpression baseedit name
bisubstitutesSealedExpression [] expr = expr
bisubstitutesSealedExpression (sub:subs) expr =
    bisubstitutesSealedExpression subs $
    mapSealedExpressionTypes (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr

eliminateOneSidedVars :: forall baseedit name. PinaforeExpression baseedit name -> PinaforeExpression baseedit name
eliminateOneSidedVars expr = let
    (setFromList -> posvars, setFromList -> negvars) = getExpressionVars expr
    posonlyvars :: FiniteSet _
    posonlyvars = difference posvars negvars
    negonlyvars :: FiniteSet _
    negonlyvars = difference negvars posvars
    mkbisub :: AnyWitness SymbolWitness -> PinaforeBisubstitution baseedit
    mkbisub (MkAnyWitness vn) =
        MkBisubstitution
            vn
            (contramap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
            (fmap (\_ -> error "bad bisubstitution") $ mkTypeF NilPinaforeType)
    bisubs = toList $ fmap mkbisub $ posonlyvars <> negonlyvars
    in bisubstitutesSealedExpression bisubs expr

mergeSharedVars :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeSharedVars expr = expr

mergeDuplicateVarsExpression :: PinaforeExpression baseedit name -> PinaforeExpression baseedit name
mergeDuplicateVarsExpression expr = expr

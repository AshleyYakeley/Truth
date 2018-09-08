{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type where

import GHC.TypeLits
import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Renamer
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
    LiteralPinaforeGroundType :: LiteralType t -> PinaforeGroundType baseedit '[] (Maybe t)
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

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    LimitPinaforeType :: PinaforeType baseedit polarity (LimitType polarity)
    JoinMeetPinaforeType
        :: PinaforeType baseedit polarity a
        -> PinaforeType baseedit polarity b
        -> PinaforeType baseedit polarity (JoinMeetType polarity a b)
    GroundPinaforeType
        :: PinaforeGroundType baseedit dv t
        -> DolanArguments dv (PinaforeType baseedit) polarity t ta
        -> PinaforeType baseedit polarity ta
    VarPinaforeType :: SymbolWitness name -> PinaforeType baseedit polarity (UVar name)

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
    -> DolanArguments dv (PinaforeType baseedit) polarity t ta
    -> [String]
getArgsTypeVars NilListType NilDolanArguments = []
getArgsTypeVars (ConsListType tv targs) (ConsDolanArguments arg args) =
    getArgTypeVars @baseedit @polarity tv arg <> getArgsTypeVars targs args

getTypeVars :: PinaforeType baseedit polarity t -> [String]
getTypeVars LimitPinaforeType = mempty
getTypeVars (JoinMeetPinaforeType ta tb) = getTypeVars ta <> getTypeVars tb
getTypeVars (GroundPinaforeType gt args) = getArgsTypeVars (pinaforeGroundTypeKind gt) args
getTypeVars (VarPinaforeType swit) = pure $ fromSymbolWitness swit

data BisubstitutionWitness baseedit t where
    PositiveBisubstitutionWitness
        :: SymbolWitness name
        -> PinaforeType baseedit 'PositivePolarity p
        -> BisubstitutionWitness baseedit (p -> UVar name)
    NegativeBisubstitutionWitness
        :: SymbolWitness name
        -> PinaforeType baseedit 'NegativePolarity q
        -> BisubstitutionWitness baseedit (UVar name -> q)

type PinaforeUnifier baseedit = Expression (BisubstitutionWitness baseedit)

{-
joinPinaforeArgument :: forall baseedit (sv :: SingleVariance) (a :: SingleVarianceKind sv) (b :: SingleVarianceKind sv) r.
    SingleVarianceType sv ->
    SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a ->
    SingleArgument sv (PinaforeType baseedit) 'PositivePolarity b ->
        (forall (ab :: SingleVarianceKind sv). SingleArgument sv (PinaforeType baseedit) 'PositivePolarity ab -> (SingleVarianceFunc sv a ab) -> (SingleVarianceFunc sv b ab) -> r) -> r
joinPinaforeArgument _ _ _ _ = undefined

joinPinaforeArguments :: forall baseedit dv tab ta tb a b r.
    DolanVarianceType dv ->
    DolanKindVary dv ta ->
    DolanKindVary dv tb ->
    DolanArguments dv (PinaforeType baseedit) 'PositivePolarity ta a ->
    DolanArguments dv (PinaforeType baseedit) 'PositivePolarity tb b ->
    (KindFunction (DolanVarianceKind dv) ta tab) ->
    (KindFunction (DolanVarianceKind dv) tb tab) ->
    (forall ab. DolanArguments dv (PinaforeType baseedit) 'PositivePolarity tab ab -> (a -> ab) -> (b -> ab) -> r)
    -> r
joinPinaforeArguments NilListType NilDolanKindVary NilDolanKindVary NilDolanArguments NilDolanArguments aab bab cont = cont NilDolanArguments aab bab
joinPinaforeArguments (ConsListType svt dvt) (ConsDolanKindVary tavary tadvary) (ConsDolanKindVary tbvary tbdvary) (ConsDolanArguments arga argsa) (ConsDolanArguments argb argsb) taab tbab cont =
    joinPinaforeArgument @baseedit svt arga argb $ \argab aconv bconv ->
        joinPinaforeArguments @baseedit dvt tadvary tbdvary argsa argsb (tavary aconv) (tbvary bconv) $ \argsab aab bab ->
            cont (ConsDolanArguments argab argsab) (foo aab taab) (foo bab tbab)
-}
joinPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'PositivePolarity b
    -> (forall ab. PinaforeType baseedit 'PositivePolarity ab -> (a -> ab) -> (b -> ab) -> r)
    -> r
joinPinaforeTypes LimitPinaforeType tb cont = cont tb never id
joinPinaforeTypes ta LimitPinaforeType cont = cont ta id never
joinPinaforeTypes (VarPinaforeType na) (VarPinaforeType nb) cont
    | Just Refl <- testEquality na nb = cont (VarPinaforeType na) id id
{-
joinPinaforeTypes (GroundPinaforeType gta argsa) (GroundPinaforeType gtb argsb) cont | Just (Refl,HRefl) <- testPinaforeGroundTypeEquality gta gtb = case dolanVarianceKMCategory @(->) $ pinaforeGroundTypeKind gta of
    Dict -> joinPinaforeArguments (pinaforeGroundTypeKind gta) (pinaforeGroundTypeVary gta) (pinaforeGroundTypeVary gtb) argsa argsb id id $ \argsab mapa mapb -> cont (GroundPinaforeType gta argsab) mapa mapb
-}
joinPinaforeTypes ta tb cont = cont (JoinMeetPinaforeType ta tb) join1 join2

meetPinaforeTypes ::
       forall baseedit (a :: Type) (b :: Type) r.
       PinaforeType baseedit 'NegativePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> (forall ab. PinaforeType baseedit 'NegativePolarity ab -> (ab -> a) -> (ab -> b) -> r)
    -> r
meetPinaforeTypes LimitPinaforeType tb cont = cont tb (\_ -> MkTopType) id
meetPinaforeTypes ta LimitPinaforeType cont = cont ta id (\_ -> MkTopType)
meetPinaforeTypes (VarPinaforeType na) (VarPinaforeType nb) cont
    | Just Refl <- testEquality na nb = cont (VarPinaforeType na) id id
meetPinaforeTypes ta tb cont = cont (JoinMeetPinaforeType ta tb) meet1 meet2

unifyPosNegVariance ::
       SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a
    -> SingleArgument sv (PinaforeType baseedit) 'NegativePolarity b
    -> Result Text (PinaforeUnifier baseedit (SingleVarianceFunc sv a b))
unifyPosNegVariance CovarianceType ta tb = unifyPosNegPinaforeTypes ta tb
unifyPosNegVariance ContravarianceType ta tb = do
    uba <- unifyPosNegPinaforeTypes tb ta
    return $ fmap MkCatDual uba
unifyPosNegVariance RangevarianceType (MkTypeRangeWitness tpa tqa) (MkTypeRangeWitness tpb tqb) = do
    upba <- unifyPosNegPinaforeTypes tpb tpa
    uqab <- unifyPosNegPinaforeTypes tqa tqb
    return $ MkRangeFunc <$> upba <*> uqab

unifyPosNegArguments ::
       forall baseedit dv gta gtb ta tb.
       DolanVarianceType dv
    -> DolanKindVary dv gta
    -> DolanArguments dv (PinaforeType baseedit) 'PositivePolarity gta ta
    -> DolanArguments dv (PinaforeType baseedit) 'NegativePolarity gtb tb
    -> PinaforeUnifier baseedit (KindFunction (DolanVarianceKind dv) gta gtb)
    -> Result Text (PinaforeUnifier baseedit (ta -> tb))
unifyPosNegArguments NilListType NilDolanKindVary NilDolanArguments NilDolanArguments ugtconv = return ugtconv
unifyPosNegArguments (ConsListType (svt :: SingleVarianceType sv) (dvt :: DolanVarianceType dv')) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) ugtconv = do
    usfunc <- unifyPosNegVariance svt sta stb
    let
        ff :: forall a b.
              KindFunction (DolanVarianceKind dv) gta gtb
           -> SingleVarianceFunc sv a b
           -> KindFunction (DolanVarianceKind dv') (gta a) (gtb b)
        ff (MkNestedMorphism gtconv) sfunc =
            case dolanVarianceKMCategory @(->) dvt of
                Dict -> gtconv . svm sfunc
    unifyPosNegArguments dvt dvm dta dtb $ ff <$> ugtconv <*> usfunc

unifyPosNegGroundTypes ::
       PinaforeGroundType baseedit dva gta
    -> DolanArguments dva (PinaforeType baseedit) 'PositivePolarity gta ta
    -> PinaforeGroundType baseedit dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) 'NegativePolarity gtb tb
    -> Result Text (PinaforeUnifier baseedit (ta -> tb))
unifyPosNegGroundTypes ga argsa gb argsb
    | Just (Refl, HRefl) <- testPinaforeGroundTypeEquality ga gb = let
        vkt = pinaforeGroundTypeKind ga
        in case dolanVarianceKMCategory @(->) vkt of
               Dict -> unifyPosNegArguments vkt (pinaforeGroundTypeVary ga) argsa argsb $ pure id
unifyPosNegGroundTypes ga argsa gb argsb =
    fail $
    "can't cast " <>
    (unpack $ exprShow $ GroundPinaforeType ga argsa) <> " to " <> (unpack $ exprShow $ GroundPinaforeType gb argsb)

unifyPosNegPinaforeTypes ::
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> Result Text (PinaforeUnifier baseedit (a -> b))
unifyPosNegPinaforeTypes LimitPinaforeType _ = return $ pure $ never
unifyPosNegPinaforeTypes _ LimitPinaforeType = return $ pure $ \_ -> MkTopType
unifyPosNegPinaforeTypes (JoinMeetPinaforeType t1 t2) tb = do
    uf1 <- unifyPosNegPinaforeTypes t1 tb
    uf2 <- unifyPosNegPinaforeTypes t2 tb
    return $
        (\f1 f2 (MkJoinType a12) ->
             case a12 of
                 Left a -> f1 a
                 Right a -> f2 a) <$>
        uf1 <*>
        uf2
unifyPosNegPinaforeTypes ta (JoinMeetPinaforeType t1 t2) = do
    uf1 <- unifyPosNegPinaforeTypes ta t1
    uf2 <- unifyPosNegPinaforeTypes ta t2
    return $ (\f1 f2 a -> MkMeetType (f1 a, f2 a)) <$> uf1 <*> uf2
unifyPosNegPinaforeTypes (VarPinaforeType na) (VarPinaforeType nb)
    | Just Refl <- testEquality na nb = return $ pure id
unifyPosNegPinaforeTypes (VarPinaforeType na) tb = return $ varExpression $ NegativeBisubstitutionWitness na tb
unifyPosNegPinaforeTypes ta (VarPinaforeType nb) = return $ varExpression $ PositiveBisubstitutionWitness nb ta
unifyPosNegPinaforeTypes (GroundPinaforeType gta argsa) (GroundPinaforeType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

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
    -> DolanArguments dv (PinaforeType baseedit) polarity t a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @baseedit @polarity svt n arg || occursInArgs dvt n args

occursInType :: SymbolWitness n -> PinaforeType baseedit polarity a -> Bool
occursInType _ LimitPinaforeType = False
occursInType n (JoinMeetPinaforeType t1 t2) = occursInType n t1 || occursInType n t2
occursInType n (VarPinaforeType nt)
    | Just Refl <- testEquality n nt = True
occursInType _ (VarPinaforeType _) = False
occursInType n (GroundPinaforeType gt args) = occursInArgs (pinaforeGroundTypeKind gt) n args

data Bisubstitution (wit :: TypePolarity -> Type -> Type) =
    forall name p q. MkBisubstitution (SymbolWitness name)
                                      (wit 'PositivePolarity p)
                                      (wit 'NegativePolarity q)
                                      (UVar name -> p)
                                      (q -> UVar name)

bisubstitutePositiveVariance ::
       SingleVarianceType sv
    -> Bisubstitution (PinaforeType baseedit)
    -> SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a
    -> (forall a'. SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a' -> SingleVarianceFunc sv a a' -> r)
    -> r
bisubstitutePositiveVariance CovarianceType bisub t cont = bisubstitutePositiveType bisub t cont
bisubstitutePositiveVariance ContravarianceType bisub t cont =
    bisubstituteNegativeType bisub t $ \t' conv -> cont t' $ MkCatDual conv
bisubstitutePositiveVariance RangevarianceType bisub (MkTypeRangeWitness tp tq) cont =
    bisubstituteNegativeType bisub tp $ \tp' convp ->
        bisubstitutePositiveType bisub tq $ \tq' convq -> cont (MkTypeRangeWitness tp' tq') $ MkRangeFunc convp convq

bisubstituteNegativeVariance ::
       SingleVarianceType sv
    -> Bisubstitution (PinaforeType baseedit)
    -> SingleArgument sv (PinaforeType baseedit) 'NegativePolarity a
    -> (forall a'. SingleArgument sv (PinaforeType baseedit) 'NegativePolarity a' -> SingleVarianceFunc sv a' a -> r)
    -> r
bisubstituteNegativeVariance CovarianceType bisub arg cont = bisubstituteNegativeType bisub arg cont
bisubstituteNegativeVariance ContravarianceType bisub arg cont =
    bisubstitutePositiveType bisub arg $ \arg' conv -> cont arg' $ MkCatDual conv
bisubstituteNegativeVariance RangevarianceType bisub (MkTypeRangeWitness tp tq) cont =
    bisubstitutePositiveType bisub tp $ \tp' convp ->
        bisubstituteNegativeType bisub tq $ \tq' convq -> cont (MkTypeRangeWitness tp' tq') $ MkRangeFunc convp convq

bisubstitutePositiveArgs ::
       Bisubstitution (PinaforeType baseedit)
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv (PinaforeType baseedit) 'PositivePolarity gt t
    -> KindFunction (DolanVarianceKind dv) gt gt'
    -> (forall t'. DolanArguments dv (PinaforeType baseedit) 'PositivePolarity gt' t' -> (t -> t') -> r)
    -> r
bisubstitutePositiveArgs _ NilListType NilDolanKindVary NilDolanArguments conv cont = cont NilDolanArguments conv
bisubstitutePositiveArgs bisub (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) conv cont =
    bisubstitutePositiveVariance svt bisub sta $ \sta' svf ->
        case conv of
            MkNestedMorphism mconv ->
                case dolanVarianceKMCategory @(->) dvt of
                    Dict ->
                        bisubstitutePositiveArgs bisub dvt dvm dta (mconv . svm svf) $ \dta' conv' ->
                            cont (ConsDolanArguments sta' dta') conv'

bisubstituteNegativeArgs ::
       Bisubstitution (PinaforeType baseedit)
    -> DolanVarianceType dv
    -> DolanKindVary dv gt
    -> DolanArguments dv (PinaforeType baseedit) 'NegativePolarity gt t
    -> KindFunction (DolanVarianceKind dv) gt' gt
    -> (forall t'. DolanArguments dv (PinaforeType baseedit) 'NegativePolarity gt' t' -> (t' -> t) -> r)
    -> r
bisubstituteNegativeArgs _ NilListType NilDolanKindVary NilDolanArguments conv cont = cont NilDolanArguments conv
bisubstituteNegativeArgs bisub (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) conv cont =
    bisubstituteNegativeVariance svt bisub sta $ \sta' svf ->
        case conv of
            MkNestedMorphism mconv ->
                case dolanVarianceKMCategory @(->) dvt of
                    Dict ->
                        bisubstituteNegativeArgs bisub dvt dvm dta (svm svf . mconv) $ \dta' conv' ->
                            cont (ConsDolanArguments sta' dta') conv'

bisubstitutePositiveType ::
       Bisubstitution (PinaforeType baseedit)
    -> PinaforeType baseedit 'PositivePolarity t
    -> (forall t'. PinaforeType baseedit 'PositivePolarity t' -> (t -> t') -> r)
    -> r
bisubstitutePositiveType _ LimitPinaforeType cont = cont LimitPinaforeType id
bisubstitutePositiveType bisub (JoinMeetPinaforeType ta tb) cont =
    bisubstitutePositiveType bisub ta $ \ta' conva ->
        bisubstitutePositiveType bisub tb $ \tb' convb -> cont (JoinMeetPinaforeType ta' tb') $ joinBimap conva convb
bisubstitutePositiveType (MkBisubstitution n tp _ convp _) (VarPinaforeType n') cont
    | Just Refl <- testEquality n n' = cont tp convp
bisubstitutePositiveType _ t@(VarPinaforeType _) cont = cont t id
bisubstitutePositiveType bisub (GroundPinaforeType gt args) cont = let
    dvt = pinaforeGroundTypeKind gt
    in case dolanVarianceKMCategory @(->) dvt of
           Dict ->
               bisubstitutePositiveArgs bisub dvt (pinaforeGroundTypeVary gt) args id $ \args' conv ->
                   cont (GroundPinaforeType gt args') conv

bisubstituteNegativeType ::
       Bisubstitution (PinaforeType baseedit)
    -> PinaforeType baseedit 'NegativePolarity t
    -> (forall t'. PinaforeType baseedit 'NegativePolarity t' -> (t' -> t) -> r)
    -> r
bisubstituteNegativeType _ LimitPinaforeType cont = cont LimitPinaforeType id
bisubstituteNegativeType bisub (JoinMeetPinaforeType ta tb) cont =
    bisubstituteNegativeType bisub ta $ \ta' conva ->
        bisubstituteNegativeType bisub tb $ \tb' convb -> cont (JoinMeetPinaforeType ta' tb') $ meetBimap conva convb
bisubstituteNegativeType (MkBisubstitution n _ tq _ convq) (VarPinaforeType n') cont
    | Just Refl <- testEquality n n' = cont tq convq
bisubstituteNegativeType _ t@(VarPinaforeType _) cont = cont t id
bisubstituteNegativeType bisub (GroundPinaforeType gt args) cont = let
    dvt = pinaforeGroundTypeKind gt
    in case dolanVarianceKMCategory @(->) dvt of
           Dict ->
               bisubstituteNegativeArgs bisub dvt (pinaforeGroundTypeVary gt) args id $ \args' conv ->
                   cont (GroundPinaforeType gt args') conv

bisubstituteUnifier ::
       Bisubstitution (PinaforeType baseedit) -> PinaforeUnifier baseedit a -> Result Text (PinaforeUnifier baseedit a)
bisubstituteUnifier _ (ClosedExpression a) = return $ ClosedExpression a
bisubstituteUnifier bisub@(MkBisubstitution bn _ tq _ qconv) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        uconv <- unifyPosNegPinaforeTypes tp tq
        uval' <- bisubstituteUnifier bisub uval
        return $ (\conv val -> val $ qconv . conv) <$> uconv <*> uval'
bisubstituteUnifier bisub@(MkBisubstitution bn tp _ pconv _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        uconv <- unifyPosNegPinaforeTypes tp tq
        uval' <- bisubstituteUnifier bisub uval
        return $ (\conv val -> val $ conv . pconv) <$> uconv <*> uval'
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) =
    bisubstitutePositiveType bisub tp $ \tp' conv -> do
        uval' <- bisubstituteUnifier bisub uval
        return $ OpenExpression (PositiveBisubstitutionWitness vn tp') $ fmap (\ca pv -> ca $ (pv . conv)) uval'
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) =
    bisubstituteNegativeType bisub tp $ \tp' conv -> do
        uval' <- bisubstituteUnifier bisub uval
        return $ OpenExpression (NegativeBisubstitutionWitness vn tp') $ fmap (\ca pv -> ca $ (conv . pv)) uval'

runUnifier ::
       forall baseedit t a r.
       PinaforeType baseedit 'PositivePolarity t
    -> PinaforeUnifier baseedit a
    -> (forall t'. PinaforeType baseedit 'PositivePolarity t' -> (t -> t') -> a -> Result Text r)
    -> Result Text r
runUnifier t (ClosedExpression a) cont = cont t id a
runUnifier _ (OpenExpression (PositiveBisubstitutionWitness vn tp) _) _
    | occursInType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier _ (OpenExpression (NegativeBisubstitutionWitness vn tp) _) _
    | occursInType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier t (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolWitness name) (tp :: PinaforeType baseedit 'PositivePolarity vw)) expr) cont = let
    varBij :: Bijection (JoinType vw (UVar name)) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (JoinMeetPinaforeType tp (VarPinaforeType vn))
            (VarPinaforeType vn)
            (biBackwards varBij)
            (biForwards varBij . join2)
    in do
           expr' <- bisubstituteUnifier bisub expr
           bisubstitutePositiveType bisub t $ \t' conv ->
               runUnifier t' expr' $ \t'' convt ca -> cont t'' (convt . conv) (ca $ biForwards varBij . join1)
runUnifier t (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolWitness name) (tq :: PinaforeType baseedit 'NegativePolarity vw)) expr) cont = let
    varBij :: Bijection (MeetType vw (UVar name)) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (VarPinaforeType vn)
            (JoinMeetPinaforeType tq (VarPinaforeType vn))
            (meet2 . biBackwards varBij)
            (biForwards varBij)
    in do
           expr' <- bisubstituteUnifier bisub expr
           bisubstitutePositiveType bisub t $ \t' conv ->
               runUnifier t' expr' $ \t'' convt ca -> cont t'' (convt . conv) (ca $ meet1 . biBackwards varBij)

instance Unifier (PinaforeUnifier baseedit) where
    type UnifierMonad (PinaforeUnifier baseedit) = Result Text
    type UnifierNegWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'NegativePolarity
    type UnifierPosWitness (PinaforeUnifier baseedit) = PinaforeType baseedit 'PositivePolarity
    unifyNegWitnesses ta tb cont = meetPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosWitnesses ta tb cont = joinPinaforeTypes ta tb $ \tab conva convb -> cont tab $ pure (conva, convb)
    unifyPosNegWitnesses = unifyPosNegPinaforeTypes
    solveUnifier t expr cont =
        runUnifier t expr $ \t' conv a -> simplifyType t' $ \t'' conv' -> cont t'' (conv' . conv) a

type PinaforeTypeNamespace baseedit (w :: k -> Type)
     = forall t1 r.
               w t1 -> (forall t2. w t2 -> KindBijection k t1 t2 -> VarNamespace (PinaforeTypeSystem baseedit) r) -> VarNamespace (PinaforeTypeSystem baseedit) r

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

-- type LiftBijection (f :: kp -> kq) = forall (a :: kp) (b :: kp). KindBijection kp a b -> KindBijection kq (f a) (f b)
{-
vcBijection ::
       forall (v :: SingleVariance) k (f :: SingleVarianceKind v -> k). HasKindMorphism k
    => SingleVarianceType v
    -> SingleVarianceMap v f
    -> LiftBijection f
-}
renameTypeArgs ::
       forall baseedit (polarity :: TypePolarity) (dv :: DolanVariance) (t :: DolanVarianceKind dv).
       IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanKindVary dv t
    -> PinaforeTypeNamespace baseedit (DolanArguments dv (PinaforeType baseedit) polarity t)
renameTypeArgs NilListType NilDolanKindVary NilDolanArguments cont = cont NilDolanArguments id
renameTypeArgs (ConsListType svt dvt) (ConsDolanKindVary svm dvm) (ConsDolanArguments arg args) cont =
    renameTypeArg @baseedit @polarity svt arg $ \arg' bijarg ->
        case dolanVarianceHasKM dvt of
            Dict ->
                bijectTypeArguments (vcBijection svt svm bijarg) args $ \args' bijargs ->
                    renameTypeArgs dvt dvm args' $ \args'' bijargs' ->
                        cont (ConsDolanArguments arg' args'') $ bijargs' . bijargs

renamePinaforeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeType baseedit polarity)
renamePinaforeTypeVars LimitPinaforeType cont = cont LimitPinaforeType id
renamePinaforeTypeVars (JoinMeetPinaforeType ta tb) cont =
    renamePinaforeTypeVars ta $ \ta' bija ->
        renamePinaforeTypeVars tb $ \tb' bijb -> cont (JoinMeetPinaforeType ta' tb') $ jmBiMap @polarity bija bijb
renamePinaforeTypeVars (GroundPinaforeType gt args) cont =
    renameTypeArgs @baseedit @polarity (pinaforeGroundTypeKind gt) (pinaforeGroundTypeVary gt) args $ \args' bij ->
        cont (GroundPinaforeType gt args') bij
renamePinaforeTypeVars (VarPinaforeType namewit1) cont =
    renameUVar varNamespaceRename namewit1 $ \namewit2 bij -> cont (VarPinaforeType namewit2) bij

data PinaforeTypeSystem (baseedit :: Type)

functionWitness ::
       PinaforeType baseedit (InvertPolarity polarity) ta
    -> PinaforeType baseedit polarity tb
    -> PinaforeType baseedit polarity (ta -> tb)
functionWitness ta tb =
    GroundPinaforeType FuncPinaforeGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance Namespace (VarNamespace (PinaforeTypeSystem baseedit)) where
    type NamespaceNegWitness (VarNamespace (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'NegativePolarity
    type NamespacePosWitness (VarNamespace (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'PositivePolarity
    renameNegWitness = renamePinaforeTypeVars
    renamePosWitness = renamePinaforeTypeVars

instance Renamer (VarRenamer (PinaforeTypeSystem baseedit)) where
    type RenamerNamespace (VarRenamer (PinaforeTypeSystem baseedit)) = VarNamespace (PinaforeTypeSystem baseedit)
    renameNewVar cont = do
        n <- varRenamerGenerate
        toSymbolWitness n $ \wit -> cont (VarPinaforeType wit) (VarPinaforeType wit)
    namespace = runVarNamespace
    runRenamer = runVarRenamer

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TypeRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TypeUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type NegWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'NegativePolarity
    type PosWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'PositivePolarity
    type TSMonad (PinaforeTypeSystem baseedit) = Result Text
    typeSystemFunctionPosWitness ta tb cont =
        cont
            (GroundPinaforeType FuncPinaforeGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments)
            id
    typeSystemFunctionNegWitness ta tb cont =
        cont
            (GroundPinaforeType FuncPinaforeGroundType $ ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments)
            id

simplifyType ::
       forall baseedit polarity a r. IsTypePolarity polarity
    => PinaforeType baseedit polarity a
    -> (forall b. PinaforeType baseedit polarity b -> (a -> b) -> r)
    -> r
simplifyType (JoinMeetPinaforeType LimitPinaforeType tb) cont = cont tb $ biForwards $ jmLeftIdentity @polarity
simplifyType (JoinMeetPinaforeType ta LimitPinaforeType) cont = cont ta $ biForwards $ jmRightIdentity @polarity
simplifyType t cont = cont t id

instance IsTypePolarity polarity => ExprShow (PinaforeType baseedit polarity t) where
    exprShowPrec LimitPinaforeType = (showLimitType @polarity, 0)
    exprShowPrec (JoinMeetPinaforeType ta tb) =
        (exprPrecShow 2 ta <> " " <> showJoinMeetType @polarity <> " " <> exprPrecShow 2 tb, 3)
    exprShowPrec (VarPinaforeType namewit) = (pack $ show namewit, 0)
    exprShowPrec (GroundPinaforeType gt args) = exprShowPrecGroundType gt args

instance IsTypePolarity polarity => Show (PinaforeType baseedit polarity t) where
    show v = unpack $ exprShow v

exprShowPrecGroundType ::
       forall baseedit polarity dv t ta. IsTypePolarity polarity
    => PinaforeGroundType baseedit dv t
    -> DolanArguments dv (PinaforeType baseedit) polarity t ta
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

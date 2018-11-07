{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type
    ( module Pinafore.Language.Type
    , module Pinafore.Language.GroundType
    , module Language.Expression.UVar
    , module Language.Expression.Dolan
    , module Pinafore.Language.TypeContext
    ) where

import qualified Data.List as List
import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.Typed
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Base
import Pinafore.Language.GroundType
import Pinafore.Language.Literal
import Pinafore.Language.NamedEntity
import Pinafore.Language.Show
import Pinafore.Language.TypeContext
import Shapes

type PinaforeRangeType baseedit = RangeType (PinaforeType baseedit)

data PinaforeType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    NilPinaforeType :: PinaforeType baseedit polarity (LimitType polarity)
    ConsPinaforeType
        :: PinaforeSingularType baseedit polarity t
        -> PinaforeType baseedit polarity tr
        -> PinaforeType baseedit polarity (JoinMeetType polarity t tr)

singlePinaforeType ::
       PinaforeSingularType baseedit polarity t
    -> PinaforeType baseedit polarity (JoinMeetType polarity t (LimitType polarity))
singlePinaforeType st = ConsPinaforeType st NilPinaforeType

literalPinaforeType :: LiteralType t -> PinaforeType baseedit polarity (JoinMeetType polarity t (LimitType polarity))
literalPinaforeType t = singlePinaforeType $ GroundPinaforeSingularType (LiteralPinaforeGroundType t) NilDolanArguments

type PinaforeTypeF (baseedit :: Type) = TypeF (PinaforeType baseedit)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
data PinaforeSingularType (baseedit :: Type) (polarity :: TypePolarity) (t :: Type) where
    GroundPinaforeSingularType
        :: PinaforeGroundType baseedit polarity dv t
        -> DolanArguments dv (PinaforeType baseedit) t polarity ta
        -> PinaforeSingularType baseedit polarity ta
    VarPinaforeSingularType :: SymbolWitness name -> PinaforeSingularType baseedit polarity (UVar name)

singlePinaforeTypeF ::
       forall baseedit polarity t. IsTypePolarity polarity
    => TypeF (PinaforeSingularType baseedit) polarity t
    -> PinaforeTypeF baseedit polarity t
singlePinaforeTypeF (MkTypeF st conv) =
    case whichTypePolarity @polarity of
        Left Refl -> contramap conv $ MkTypeF (singlePinaforeType st) join1
        Right Refl -> fmap conv $ MkTypeF (singlePinaforeType st) meet1

type LiftBijection (f :: kp -> kq) = forall (a :: kp) (b :: kp). KindBijection kp a b -> KindBijection kq (f a) (f b)

vcBijection ::
       forall (v :: SingleVariance) k (f :: SingleVarianceKind v -> k). HasKindMorphism k
    => SingleVarianceType v
    -> SingleVarianceMap v f
    -> LiftBijection f
vcBijection CovarianceType conv (MkBijection ab ba) = mkKindBijection (conv ab) (conv ba)
vcBijection ContravarianceType conv (MkBijection ba ab) = mkKindBijection (conv $ MkCatDual ab) (conv $ MkCatDual ba)
vcBijection RangevarianceType conv (MkPairMorphism (MkBijection papb pbpa) (MkBijection qaqb qbqa)) =
    mkKindBijection (conv $ MkWithRange pbpa qaqb) (conv $ MkWithRange papb qbqa)

getRangeTypeVars :: PinaforeRangeType baseedit polarity t -> [String]
getRangeTypeVars (MkRangeType tp tq) = getTypeVars tp <> getTypeVars tq

getArgTypeVars ::
       forall baseedit polarity v a.
       SingleVarianceType v
    -> SingleArgument v (PinaforeType baseedit) polarity a
    -> [String]
getArgTypeVars CovarianceType ft = getTypeVars ft
getArgTypeVars ContravarianceType ft = getTypeVars ft
getArgTypeVars RangevarianceType (MkRangeType ta tb) = getTypeVars ta <> getTypeVars tb

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

type PinaforeFullUnifier baseedit
     = Compose (VarRenamer (PinaforeTypeSystem baseedit) PinaforeTypeCheck) (PinaforeUnifier baseedit)

unifierLiftTypeCheck :: PinaforeTypeCheck a -> PinaforeFullUnifier baseedit a
unifierLiftTypeCheck tca = Compose $ fmap pure $ lift tca

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

instance IsTypePolarity polarity => Semigroup (AnyW (PinaforeType baseedit polarity)) where
    MkAnyW ta <> MkAnyW tb =
        case whichTypePolarity @polarity of
            Left Refl -> joinPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab
            Right Refl -> meetPinaforeTypes ta tb $ \tab _ _ -> MkAnyW tab

instance IsTypePolarity polarity => Monoid (AnyW (PinaforeType baseedit polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilPinaforeType

instance IsTypePolarity polarity => Semigroup (AnyInKind (RangeType (PinaforeType baseedit) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance IsTypePolarity polarity => Monoid (AnyInKind (RangeType (PinaforeType baseedit) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilPinaforeType NilPinaforeType)

unifyPosNegVariance ::
       SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) 'PositivePolarity a
    -> SingleArgument sv (PinaforeType baseedit) 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (SingleVarianceFunc sv a b)
unifyPosNegVariance CovarianceType ta tb = unifyPosNegPinaforeTypes ta tb
unifyPosNegVariance ContravarianceType ta tb = do
    ba <- unifyPosNegPinaforeTypes tb ta
    return $ MkCatDual ba
unifyPosNegVariance RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- unifyPosNegPinaforeTypes tpb tpa
    qab <- unifyPosNegPinaforeTypes tqa tqb
    return $ MkWithRange pba qab

unifyPosNegArguments' ::
       forall baseedit dv gta gtb ta tb.
       DolanVarianceType dv
    -> DolanKindVary dv gta
    -> DolanArguments dv (PinaforeType baseedit) gta 'PositivePolarity ta
    -> DolanArguments dv (PinaforeType baseedit) gtb 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (KindFunction (DolanVarianceKind dv) gta gtb -> ta -> tb)
unifyPosNegArguments' NilListType NilDolanKindVary NilDolanArguments NilDolanArguments = pure id
unifyPosNegArguments' (ConsListType (svt :: SingleVarianceType sv) (dvt :: DolanVarianceType dv')) (ConsDolanKindVary svm dvm) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) = do
    sfunc <- unifyPosNegVariance svt sta stb
    f <- unifyPosNegArguments' dvt dvm dta dtb
    pure $
        case dolanVarianceKMCategory @(->) dvt of
            Dict -> \(MkNestedMorphism conv) -> f (conv . svm sfunc)

unifyPosNegArguments ::
       forall baseedit polarity dv gt ta tb.
       PinaforeGroundType baseedit polarity dv gt
    -> DolanArguments dv (PinaforeType baseedit) gt 'PositivePolarity ta
    -> DolanArguments dv (PinaforeType baseedit) gt 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (ta -> tb)
unifyPosNegArguments gt argsa argsb = let
    vkt = pinaforeGroundTypeKind gt
    in case dolanVarianceKMCategory @(->) vkt of
           Dict -> do
               f <- unifyPosNegArguments' vkt (pinaforeGroundTypeVary gt) argsa argsb
               return $ f id

unifyPosNegGroundTypes ::
       PinaforeGroundType baseedit 'PositivePolarity dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta 'PositivePolarity ta
    -> PinaforeGroundType baseedit 'NegativePolarity dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (ta -> tb)
unifyPosNegGroundTypes ActionPinaforeGroundType NilDolanArguments ActionPinaforeGroundType NilDolanArguments = pure id
unifyPosNegGroundTypes OrderPinaforeGroundType argsa OrderPinaforeGroundType argsb =
    unifyPosNegArguments OrderPinaforeGroundType argsa argsb
unifyPosNegGroundTypes UserInterfacePinaforeGroundType NilDolanArguments UserInterfacePinaforeGroundType NilDolanArguments =
    pure id
unifyPosNegGroundTypes (LiteralPinaforeGroundType la) NilDolanArguments (LiteralPinaforeGroundType lb) NilDolanArguments
    | Just conv <- isSubtype la lb = pure conv
unifyPosNegGroundTypes PointPinaforeGroundType NilDolanArguments PointPinaforeGroundType NilDolanArguments = pure id
unifyPosNegGroundTypes PointPinaforeGroundType NilDolanArguments EntityPinaforeGroundType NilDolanArguments =
    pure pointToEntity
unifyPosNegGroundTypes PointPinaforeGroundType NilDolanArguments (NamedEntityPinaforeGroundType _) NilDolanArguments =
    pure $ MkNamedEntity
unifyPosNegGroundTypes EntityPinaforeGroundType NilDolanArguments EntityPinaforeGroundType NilDolanArguments = pure id
unifyPosNegGroundTypes (NamedEntityPinaforeGroundType t1) NilDolanArguments (NamedEntityPinaforeGroundType t2) NilDolanArguments =
    unifierLiftTypeCheck $ getEntitySubtype t1 t2
unifyPosNegGroundTypes (NamedEntityPinaforeGroundType _) NilDolanArguments EntityPinaforeGroundType NilDolanArguments =
    pure namedToEntity
unifyPosNegGroundTypes (LiteralPinaforeGroundType tl) NilDolanArguments EntityPinaforeGroundType NilDolanArguments =
    pure $
    case literalTypeAsLiteral tl of
        Dict -> pointToEntity . literalToPoint
unifyPosNegGroundTypes PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) EntityPinaforeGroundType NilDolanArguments =
    (\conva convb (a, b) -> pairToEntity (meet1 $ conva a, meet1 $ convb b)) <$>
    unifyPosNegPinaforeTypes
        ta
        (singlePinaforeType $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments) <*>
    unifyPosNegPinaforeTypes
        tb
        (singlePinaforeType $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments)
unifyPosNegGroundTypes FuncPinaforeGroundType argsa FuncPinaforeGroundType argsb =
    unifyPosNegArguments FuncPinaforeGroundType argsa argsb
unifyPosNegGroundTypes ListPinaforeGroundType argsa ListPinaforeGroundType argsb =
    unifyPosNegArguments ListPinaforeGroundType argsa argsb
unifyPosNegGroundTypes PairPinaforeGroundType argsa PairPinaforeGroundType argsb =
    unifyPosNegArguments PairPinaforeGroundType argsa argsb
unifyPosNegGroundTypes ReferencePinaforeGroundType argsa ReferencePinaforeGroundType argsb =
    unifyPosNegArguments ReferencePinaforeGroundType argsa argsb
unifyPosNegGroundTypes SetPinaforeGroundType argsa SetPinaforeGroundType argsb =
    unifyPosNegArguments SetPinaforeGroundType argsa argsb
unifyPosNegGroundTypes MorphismPinaforeGroundType argsa MorphismPinaforeGroundType argsb =
    unifyPosNegArguments MorphismPinaforeGroundType argsa argsb
unifyPosNegGroundTypes ga argsa gb argsb =
    unifierLiftTypeCheck $
    convertFailure
        (unpack $ exprShow $ GroundPinaforeSingularType ga argsa)
        (unpack $ exprShow $ GroundPinaforeSingularType gb argsb)

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
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

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
bisubstitutePositiveSingularType _ t@(VarPinaforeSingularType _) = singlePinaforeTypeF $ mkTypeF t
bisubstitutePositiveSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in case mapDolanArguments (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args of
           MkTypeF args' conv -> singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

bisubstituteNegativeSingularType ::
       PinaforeBisubstitution baseedit
    -> PinaforeSingularType baseedit 'NegativePolarity t
    -> PinaforeTypeF baseedit 'NegativePolarity t
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarPinaforeSingularType _) = singlePinaforeTypeF $ mkTypeF t
bisubstituteNegativeSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in case mapDolanArguments (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args of
           MkTypeF args' conv -> singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

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
        conv <- unifyPosNegPinaforeTypeF (singlePinaforeTypeF $ mkTypeF tp) tq
        val' <- bisubstituteUnifier bisub uval
        return $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn tp _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyPosNegPinaforeTypeF tp (singlePinaforeTypeF $ mkTypeF tq)
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

runUnifier ::
       forall baseedit a.
       PinaforeUnifier baseedit a
    -> VarRenamer (PinaforeTypeSystem baseedit) PinaforeTypeCheck (a, [PinaforeBisubstitution baseedit])
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
                 (singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
                 (singlePinaforeTypeF $ mkTypeF tp))
            (fmap (biForwards varBij . join1) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
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
            (contramap (meet1 . biBackwards varBij) $ singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
            (fmap (biForwards varBij) $
             meetPinaforeTypeF
                 (singlePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType vn)
                 (singlePinaforeTypeF $ mkTypeF tq))
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
    type UnifierMonad (PinaforeUnifier baseedit) = VarRenamer (PinaforeTypeSystem baseedit) PinaforeTypeCheck
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

type TypeNamespace (ts :: Type) (w :: k -> Type)
     = forall t1 m r.
           Monad m =>
                   w t1 -> (forall t2. InKind t2 => w t2 -> KindBijection k t1 t2 -> VarNamespace ts (VarRenamer ts m) r) -> VarNamespace ts (VarRenamer ts m) r

type PinaforeTypeNamespace baseedit w = TypeNamespace (PinaforeTypeSystem baseedit) w

renamePinaforeRangeTypeVars ::
       forall baseedit polarity. IsTypePolarity polarity
    => PinaforeTypeNamespace baseedit (PinaforeRangeType baseedit polarity)
renamePinaforeRangeTypeVars (MkRangeType ta tb) cont =
    invertPolarity @polarity $
    renamePinaforeTypeVars ta $ \ta' bija ->
        renamePinaforeTypeVars tb $ \tb' bijb -> cont (MkRangeType ta' tb') $ MkPairMorphism bija bijb

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

instance Renamer (VarRenamer (PinaforeTypeSystem baseedit)) where
    type RenamerNamespace (VarRenamer (PinaforeTypeSystem baseedit)) = VarNamespace (PinaforeTypeSystem baseedit)
    type RenamerNegWitness (VarRenamer (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'NegativePolarity
    type RenamerPosWitness (VarRenamer (PinaforeTypeSystem baseedit)) = PinaforeType baseedit 'PositivePolarity
    renameNegWitness = renamePinaforeTypeVars
    renamePosWitness = renamePinaforeTypeVars
    renameNewVar cont = do
        n <- varRenamerGenerate
        toSymbolWitness n $ \wit ->
            cont (singlePinaforeType $ VarPinaforeSingularType wit) (singlePinaforeType $ VarPinaforeSingularType wit) $
            join1 . meet1
    namespace = runVarNamespace
    runRenamer = runVarRenamer

instance TypeSystem (PinaforeTypeSystem baseedit) where
    type TypeRenamer (PinaforeTypeSystem baseedit) = VarRenamer (PinaforeTypeSystem baseedit)
    type TypeUnifier (PinaforeTypeSystem baseedit) = PinaforeUnifier baseedit
    type NegWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'NegativePolarity
    type PosWitness (PinaforeTypeSystem baseedit) = PinaforeType baseedit 'PositivePolarity
    type TypeCheck (PinaforeTypeSystem baseedit) = PinaforeTypeCheck
    typeSystemFunctionPosWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType FuncPinaforeGroundType $
        ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
    typeSystemFunctionNegWitness ta tb =
        unTypeF $
        singlePinaforeTypeF $
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
    => PinaforeGroundType baseedit polarity dv t
    -> DolanArguments dv (PinaforeType baseedit) t polarity ta
    -> (Text, Int)
exprShowPrecGroundType InvertLimitPinaforeGroundType NilDolanArguments =
    invertPolarity @polarity (showLimitType @(InvertPolarity polarity), 0)
exprShowPrecGroundType ActionPinaforeGroundType NilDolanArguments = ("Action", 0)
exprShowPrecGroundType OrderPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    invertPolarity @polarity ("Order " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType UserInterfacePinaforeGroundType NilDolanArguments = ("UI", 0)
exprShowPrecGroundType (LiteralPinaforeGroundType t) NilDolanArguments = exprShowPrec t
exprShowPrecGroundType PointPinaforeGroundType NilDolanArguments = ("Point", 0)
exprShowPrecGroundType EntityPinaforeGroundType NilDolanArguments = ("Entity", 0)
exprShowPrecGroundType (NamedEntityPinaforeGroundType n) NilDolanArguments = (pack $ show n, 0)
exprShowPrecGroundType FuncPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " -> " <> exprPrecShow 3 tb, 3)
exprShowPrecGroundType ListPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) = ("[" <> exprShow ta <> "]", 0)
exprShowPrecGroundType PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    ("(" <> exprShow ta <> ", " <> exprShow tb <> ")", 0)
exprShowPrecGroundType ReferencePinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Ref " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType SetPinaforeGroundType (ConsDolanArguments ta NilDolanArguments) =
    ("Set " <> exprPrecShow 0 ta, 2)
exprShowPrecGroundType MorphismPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) =
    invertPolarity @polarity (exprPrecShow 2 ta <> " ~> " <> exprPrecShow 3 tb, 3)

instance IsTypePolarity polarity => ExprShow (PinaforeRangeType baseedit polarity a) where
    exprShowPrec (MkRangeType t1 t2) = let
        getpieces ::
               forall pol t. IsTypePolarity pol
            => PinaforeType baseedit pol t
            -> [Text]
        getpieces NilPinaforeType = []
        getpieces (ConsPinaforeType t tr) = exprPrecShow 0 t : getpieces tr
        contrapieces = nub $ invertPolarity @polarity $ getpieces t1
        copieces = nub $ getpieces t2
        bothpieces = List.intersect contrapieces copieces
        rcontrapieces = contrapieces List.\\ bothpieces
        rcopieces = copieces List.\\ bothpieces
        pieces :: [Text]
        pieces = bothpieces <> fmap ("-" <>) rcontrapieces <> fmap ("+" <>) rcopieces
        text :: Text
        text =
            case pieces of
                [t] -> t
                _ -> "{" <> ointercalate "," pieces <> "}"
        in (text, 0)

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
    getExpressionVars :: t -> ([AnyW SymbolWitness], [AnyW SymbolWitness])

instance IsTypePolarity polarity => GetExpressionVars (RangeType (PinaforeType baseedit) polarity a) where
    getExpressionVars (MkRangeType tp tq) = invertPolarity @polarity $ getExpressionVars tp <> getExpressionVars tq

getArgExpressionVars ::
       forall baseedit polarity sv a. IsTypePolarity polarity
    => SingleVarianceType sv
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> ([AnyW SymbolWitness], [AnyW SymbolWitness])
getArgExpressionVars CovarianceType t = getExpressionVars t
getArgExpressionVars ContravarianceType t = invertPolarity @polarity $ getExpressionVars t
getArgExpressionVars RangevarianceType t = getExpressionVars t

getArgsExpressionVars ::
       forall baseedit polarity dv gt t. IsTypePolarity polarity
    => DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseedit) gt polarity t
    -> ([AnyW SymbolWitness], [AnyW SymbolWitness])
getArgsExpressionVars NilListType NilDolanArguments = mempty
getArgsExpressionVars (ConsListType sv dv) (ConsDolanArguments arg args) =
    getArgExpressionVars @baseedit @polarity sv arg <> getArgsExpressionVars dv args

instance IsTypePolarity polarity => GetExpressionVars (PinaforeSingularType baseedit polarity t) where
    getExpressionVars (GroundPinaforeSingularType gt args) = getArgsExpressionVars (pinaforeGroundTypeKind gt) args
    getExpressionVars (VarPinaforeSingularType vn) =
        case whichTypePolarity @polarity of
            Left Refl -> ([MkAnyW vn], [])
            Right Refl -> ([], [MkAnyW vn])

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
    mkbisub :: AnyW SymbolWitness -> PinaforeBisubstitution baseedit
    mkbisub (MkAnyW vn) =
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

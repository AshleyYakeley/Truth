{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Unify where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Renamer
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Base
import Pinafore.Language.GroundType
import Pinafore.Language.Show
import Pinafore.Language.SimpleEntityType
import Pinafore.Language.Type.Rename
import Pinafore.Language.Type.Type
import Pinafore.Language.TypeContext
import Shapes

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

type UnifierConstraint baseedit
     = UnifierMonad (PinaforeUnifier baseedit) ~ VarRenamer (PinaforeTypeSystem baseedit) SourcePinaforeTypeCheck

type PinaforeFullUnifier baseedit
     = Compose (VarRenamer (PinaforeTypeSystem baseedit) SourcePinaforeTypeCheck) (PinaforeUnifier baseedit)

unifierLiftTypeCheck :: SourcePinaforeTypeCheck a -> PinaforeFullUnifier baseedit a
unifierLiftTypeCheck tca = Compose $ fmap pure $ lift tca

unifyPosNegVariance ::
       UnifierConstraint baseedit
    => SingleVarianceType sv
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
       forall baseedit dv gta gtb ta tb. UnifierConstraint baseedit
    => DolanVarianceType dv
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
       forall baseedit polarity dv gt ta tb. UnifierConstraint baseedit
    => PinaforeGroundType baseedit polarity dv gt
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
       UnifierConstraint baseedit
    => PinaforeGroundType baseedit 'PositivePolarity dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta 'PositivePolarity ta
    -> PinaforeGroundType baseedit 'NegativePolarity dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb 'NegativePolarity tb
    -> PinaforeFullUnifier baseedit (ta -> tb)
unifyPosNegGroundTypes ActionPinaforeGroundType NilDolanArguments ActionPinaforeGroundType NilDolanArguments = pure id
unifyPosNegGroundTypes OrderPinaforeGroundType argsa OrderPinaforeGroundType argsb =
    unifyPosNegArguments OrderPinaforeGroundType argsa argsb
unifyPosNegGroundTypes UserInterfacePinaforeGroundType NilDolanArguments UserInterfacePinaforeGroundType NilDolanArguments =
    pure id
unifyPosNegGroundTypes (SimpleEntityPinaforeGroundType t1) NilDolanArguments (SimpleEntityPinaforeGroundType t2) NilDolanArguments =
    unifierLiftTypeCheck $ getSubtype t1 t2
unifyPosNegGroundTypes PairPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments =
    (\conva convb (a, b) -> pairToEntity (meet1 $ conva a, meet1 $ convb b)) <$>
    unifyPosNegPinaforeTypes
        ta
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments) <*>
    unifyPosNegPinaforeTypes
        tb
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments)
unifyPosNegGroundTypes EitherPinaforeGroundType (ConsDolanArguments ta (ConsDolanArguments tb NilDolanArguments)) (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments =
    (\conva convb eab -> eitherToEntity $ either (Left . meet1 . conva) (Right . meet1 . convb) eab) <$>
    unifyPosNegPinaforeTypes
        ta
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments) <*>
    unifyPosNegPinaforeTypes
        tb
        (singlePinaforeType $
         GroundPinaforeSingularType (SimpleEntityPinaforeGroundType TopSimpleEntityType) NilDolanArguments)
unifyPosNegGroundTypes FuncPinaforeGroundType argsa FuncPinaforeGroundType argsb =
    unifyPosNegArguments FuncPinaforeGroundType argsa argsb
unifyPosNegGroundTypes ListPinaforeGroundType argsa ListPinaforeGroundType argsb =
    unifyPosNegArguments ListPinaforeGroundType argsa argsb
unifyPosNegGroundTypes PairPinaforeGroundType argsa PairPinaforeGroundType argsb =
    unifyPosNegArguments PairPinaforeGroundType argsa argsb
unifyPosNegGroundTypes EitherPinaforeGroundType argsa EitherPinaforeGroundType argsb =
    unifyPosNegArguments EitherPinaforeGroundType argsa argsb
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
       UnifierConstraint baseedit
    => PinaforeSingularType baseedit 'PositivePolarity a
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
       UnifierConstraint baseedit
    => PinaforeSingularType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes1 _ NilPinaforeType = pure alwaysTop
unifyPosNegPinaforeTypes1 ta (ConsPinaforeType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       UnifierConstraint baseedit
    => PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes NilPinaforeType _ = pure never
unifyPosNegPinaforeTypes (ConsPinaforeType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeTypeF ::
       UnifierConstraint baseedit
    => PinaforeTypeF baseedit 'PositivePolarity a
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

bisubstituteUnifier ::
       UnifierConstraint baseedit
    => PinaforeBisubstitution baseedit
    -> PinaforeUnifier baseedit a
    -> PinaforeFullUnifier baseedit a
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

bisubstitutesSealedExpression ::
       [PinaforeBisubstitution baseedit] -> PinaforeExpression baseedit name -> PinaforeExpression baseedit name
bisubstitutesSealedExpression [] expr = expr
bisubstitutesSealedExpression (sub:subs) expr =
    bisubstitutesSealedExpression subs $
    mapSealedExpressionTypes (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr

runUnifier ::
       forall baseedit a. UnifierConstraint baseedit
    => PinaforeUnifier baseedit a
    -> VarRenamer (PinaforeTypeSystem baseedit) SourcePinaforeTypeCheck (a, [PinaforeBisubstitution baseedit])
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

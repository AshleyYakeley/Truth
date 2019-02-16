{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.Type.Unify where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.GroundType
import Pinafore.Language.Show
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Subtype
import Pinafore.Language.Type.Type
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
    getArgsTypeVars (pinaforeGroundTypeVarianceType gt) args <> getTypeVars tr
getTypeVars (ConsPinaforeType (VarPinaforeSingularType swit) tr) = fromSymbolType swit : getTypeVars tr

data BisubstitutionWitness baseedit t where
    PositiveBisubstitutionWitness
        :: SymbolType name
        -> PinaforeSingularType baseedit 'Positive p
        -> BisubstitutionWitness baseedit (p -> UVar name)
    NegativeBisubstitutionWitness
        :: SymbolType name
        -> PinaforeSingularType baseedit 'Negative q
        -> BisubstitutionWitness baseedit (UVar name -> q)

type PinaforeUnifier baseedit = Expression (BisubstitutionWitness baseedit)

type UnifierConstraint baseedit = UnifierMonad (PinaforeUnifier baseedit) ~ PinaforeTypeCheck baseedit

type PinaforeFullUnifier baseedit = Compose (PinaforeTypeCheck baseedit) (PinaforeUnifier baseedit)

fullUnifierLiftTypeCheck :: PinaforeTypeCheck baseedit a -> PinaforeFullUnifier baseedit a
fullUnifierLiftTypeCheck tca = Compose $ fmap pure tca

fullUnifierLiftSourceScoped :: PinaforeSourceScoped baseedit a -> PinaforeFullUnifier baseedit a
fullUnifierLiftSourceScoped tca = fullUnifierLiftTypeCheck $ lift tca

unifySubtypeContext ::
       UnifierConstraint baseedit => SubtypeContext baseedit (PinaforeFullUnifier baseedit) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyPosNegPinaforeTypes
    subtypeLift = fullUnifierLiftSourceScoped
    subtypeInverted = unifySubtypeContext
    in MkSubtypeContext {..}

unifyPosNegGroundTypes ::
       UnifierConstraint baseedit
    => PinaforeGroundType baseedit 'Positive dva gta
    -> DolanArguments dva (PinaforeType baseedit) gta 'Positive ta
    -> PinaforeGroundType baseedit 'Negative dvb gtb
    -> DolanArguments dvb (PinaforeType baseedit) gtb 'Negative tb
    -> PinaforeFullUnifier baseedit (ta -> tb)
unifyPosNegGroundTypes = subtypeGroundTypes unifySubtypeContext

unifyPosNegPinaforeSingularTypes ::
       UnifierConstraint baseedit
    => PinaforeSingularType baseedit 'Positive a
    -> PinaforeSingularType baseedit 'Negative b
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
    => PinaforeSingularType baseedit 'Positive a
    -> PinaforeType baseedit 'Negative b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes1 _ NilPinaforeType = pure alwaysTop
unifyPosNegPinaforeTypes1 ta (ConsPinaforeType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       UnifierConstraint baseedit
    => PinaforeType baseedit 'Positive a
    -> PinaforeType baseedit 'Negative b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypes NilPinaforeType _ = pure never
unifyPosNegPinaforeTypes (ConsPinaforeType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeTypeF ::
       UnifierConstraint baseedit
    => PinaforeTypeF baseedit 'Positive a
    -> PinaforeTypeF baseedit 'Negative b
    -> PinaforeFullUnifier baseedit (a -> b)
unifyPosNegPinaforeTypeF (MkTypeF ta conva) (MkTypeF tb convb) = do
    conv <- unifyPosNegPinaforeTypes ta tb
    return $ convb . conv . conva

occursInArg ::
       forall baseedit polarity n sv a.
       SingleVarianceType sv
    -> SymbolType n
    -> SingleArgument sv (PinaforeType baseedit) polarity a
    -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall baseedit polarity n dv t a.
       DolanVarianceType dv
    -> SymbolType n
    -> DolanArguments dv (PinaforeType baseedit) t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @baseedit @polarity svt n arg || occursInArgs dvt n args

occursInSingularType :: SymbolType n -> PinaforeSingularType baseedit polarity a -> Bool
occursInSingularType n (VarPinaforeSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarPinaforeSingularType _) = False
occursInSingularType n (GroundPinaforeSingularType gt args) = occursInArgs (pinaforeGroundTypeVarianceType gt) n args

occursInType :: SymbolType n -> PinaforeType baseedit polarity a -> Bool
occursInType _ NilPinaforeType = False
occursInType n (ConsPinaforeType t1 t2) = occursInSingularType n t1 || occursInType n t2

bisubstitutePositiveVar ::
       SymbolType name -> PinaforeType baseedit 'Positive t -> PinaforeUnifier baseedit (t -> UVar name)
bisubstitutePositiveVar _ NilPinaforeType = pure never
bisubstitutePositiveVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (PositiveBisubstitutionWitness vn t1) $ fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       SymbolType name -> PinaforeType baseedit 'Negative t -> PinaforeUnifier baseedit (UVar name -> t)
bisubstituteNegativeVar _ NilPinaforeType = pure alwaysTop
bisubstituteNegativeVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (NegativeBisubstitutionWitness vn t1) $ fmap (\fr f1 -> meetf f1 fr) $ bisubstituteNegativeVar vn tr

bisubstituteUnifier ::
       UnifierConstraint baseedit
    => PinaforeBisubstitution baseedit
    -> PinaforeUnifier baseedit a
    -> PinaforeFullUnifier baseedit a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ mtq) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tq <- mtq
                getCompose $ unifyPosNegPinaforeTypeF (singlePinaforeTypeF $ mkPTypeF tp) tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn mtp _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tp <- mtp
                getCompose $ unifyPosNegPinaforeTypeF tp (singlePinaforeTypeF $ mkPTypeF tq)
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) =
    Compose $ do
        MkTypeF tp' conv <- bisubstitutePositiveSingularType bisub tp
        uval' <- getCompose $ bisubstituteUnifier bisub uval
        return $ do
            val' <- uval'
            pv <- bisubstitutePositiveVar vn tp'
            pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) =
    Compose $ do
        MkTypeF tp' conv <- bisubstituteNegativeSingularType bisub tp
        uval' <- getCompose $ bisubstituteUnifier bisub uval
        return $ do
            val' <- uval'
            pv <- bisubstituteNegativeVar vn tp'
            pure $ val' $ conv . pv

runUnifier ::
       forall baseedit a. UnifierConstraint baseedit
    => PinaforeUnifier baseedit a
    -> PinaforeTypeCheck baseedit (a, [PinaforeBisubstitution baseedit])
runUnifier (ClosedExpression a) = return (a, [])
runUnifier (OpenExpression (PositiveBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier (OpenExpression (NegativeBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = fail $ "can't construct recursive type " <> show vn <> " = " <> unpack (exprShow tp)
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolType name) (tp :: PinaforeSingularType baseedit 'Positive vw)) expr) = let
    varBij :: Bijection (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (return $
             contramap (biBackwards varBij) $
             joinPinaforeTypeF
                 (singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
                 (singlePinaforeTypeF $ mkPTypeF tp))
            (return $ fmap (biForwards varBij . join1) $ singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ biForwards varBij . join2, bisub : subs)
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolType name) (tq :: PinaforeSingularType baseedit 'Negative vw)) expr) = let
    varBij :: Bijection (MeetType (UVar name) vw) (UVar name)
    varBij = unsafeUVarBijection
    bisub =
        MkBisubstitution
            vn
            (return $
             contramap (meet1 . biBackwards varBij) $ singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
            (return $
             fmap (biForwards varBij) $
             meetPinaforeTypeF
                 (singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
                 (singlePinaforeTypeF $ mkPTypeF tq))
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ meet2 . biBackwards varBij, bisub : subs)

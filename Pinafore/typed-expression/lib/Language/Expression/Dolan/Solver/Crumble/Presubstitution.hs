module Language.Expression.Dolan.Solver.Crumble.Presubstitution
    ( Presubstitution
    , assignPresubstitution
    , presubstitute
    , preBisubstitution
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

data SubShimWit (ground :: GroundTypeKind) (polarity :: Polarity) oldtv newtv
    = forall t. MkSubShimWit
        (DolanType ground polarity t)
        (DolanPolarShim ground polarity oldtv (JoinMeetType polarity newtv t))

instance
    forall (ground :: GroundTypeKind) polarity oldtv newtv.
    (ShowGroundType ground, Is PolarityType polarity) =>
    Show (SubShimWit ground polarity oldtv newtv)
    where
    show (MkSubShimWit t _) = allShow t

isCleanType ::
    forall (ground :: GroundTypeKind) polarity oldtv a.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT oldtv ->
    DolanType ground polarity a ->
    Bool
isCleanType _ NilDolanType = True
isCleanType var (ConsDolanType (VarDolanSingularType v) _)
    | Just Refl <- testEquality var v = False
isCleanType var (ConsDolanType _ t) = isCleanType var t

toVarType ::
    forall (ground :: GroundTypeKind) polarity oldtv a r.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT oldtv ->
    DolanType ground polarity a ->
    ( forall a'.
      DolanType ground polarity a' -> DolanPolarShim ground polarity a (JoinMeetType polarity a' oldtv) -> r
    ) ->
    r
toVarType _ NilDolanType call = call NilDolanType polar1
toVarType var (ConsDolanType (VarDolanSingularType v) t) call
    | Just Refl <- testEquality var v = toVarType var t $ \t' conv -> call t' $ polarF polar2 conv
toVarType var (ConsDolanType st t) call =
    toVarType var t $ \t' conv -> call (ConsDolanType st t') $ iPolarSwapR . iPolarPair id conv

cleanSubShimWit ::
    forall (ground :: GroundTypeKind) polarity oldtv newtv.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    TypeVarT oldtv ->
    SubShimWit ground polarity oldtv newtv ->
    SubShimWit ground polarity oldtv newtv
cleanSubShimWit var (MkSubShimWit t conv) =
    if isCleanType var t
        then MkSubShimWit t conv
        else toVarType var t $ \t' vconv -> let
            rconv = polarF polar1 (polarF polar2 (lazyPolarShim rconv) . vconv) . conv
            in MkSubShimWit t' rconv

mixedToPlain ::
    forall (ground :: GroundTypeKind) polarity t.
    (IsDolanGroundType ground, Is PolarityType polarity) =>
    MixedType ground polarity t ->
    TypeResult ground (DolanShimWit ground polarity t)
mixedToPlain (MkMixedType t NilInvertedType) = return $ MkShimWit t iPolarL1
mixedToPlain (MkMixedType _ (ConsInvertedType invt _)) =
    withInvertPolarity @polarity $ throw $ UninvertibleTypeError invt

assignPresubstitution ::
    forall (ground :: GroundTypeKind) v a b.
    IsDolanGroundType ground =>
    TypeVarT v ->
    MixedType ground 'Positive a ->
    MixedType ground 'Negative b ->
    DolanTypeCheckM
        ground
        ( Presubstitution ground
        , DolanShim ground a b -> (DolanShim ground a v, DolanShim ground v b)
        )
assignPresubstitution oldvar mta mtb = do
    MkSomeTypeVarT (newvar :: TypeVarT newtv) <- renamerGenerateFreeTypeVarT
    return
        $ assignTypeVarT @(MeetType (JoinType newtv a) b) oldvar
        $ let
            toSubShimWitPos ::
                forall.
                DolanShimWit ground 'Positive a ->
                SubShimWit ground 'Positive (MeetType (JoinType newtv a) b) newtv
            toSubShimWitPos (MkShimWit t (MkPolarShim conv)) = MkSubShimWit t $ MkPolarShim $ iJoinPair id conv . meet1
            toSubShimWitNeg ::
                forall.
                DolanShimWit ground 'Negative b ->
                SubShimWit ground 'Negative (MeetType (JoinType newtv a) b) newtv
            toSubShimWitNeg (MkShimWit t (MkPolarShim conv)) = MkSubShimWit t $ MkPolarShim $ iMeetPair join1 conv
            msta = fmap toSubShimWitPos $ mixedToPlain mta
            mstb = fmap toSubShimWitNeg $ mixedToPlain mtb
            in (MkPresubstitution oldvar newvar msta mstb, \convab -> (meetf join2 convab, meet2))

data Presubstitution (ground :: GroundTypeKind) where
    MkPresubstitution ::
        forall (ground :: GroundTypeKind) oldtv newtv.
        TypeVarT oldtv ->
        TypeVarT newtv ->
        TypeResult ground (SubShimWit ground 'Positive oldtv newtv) ->
        TypeResult ground (SubShimWit ground 'Negative oldtv newtv) ->
        Presubstitution ground

instance forall (ground :: GroundTypeKind). ShowGroundType ground => Show (Presubstitution ground) where
    show (MkPresubstitution oldvar newvar ta tb) = let
        showM (SuccessResult t) = show t
        showM (FailureResult _) = "ERR"
        in "{"
            <> show oldvar
            <> "+ => "
            <> show newvar
            <> " | "
            <> showM ta
            <> "; "
            <> show oldvar
            <> "- => "
            <> show newvar
            <> " & "
            <> showM tb
            <> "}"

cleanPresubstitution ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Presubstitution ground ->
    Presubstitution ground
cleanPresubstitution (MkPresubstitution oldvar newvar msta mstb) =
    MkPresubstitution oldvar newvar (fmap (cleanSubShimWit oldvar) msta) (fmap (cleanSubShimWit oldvar) mstb)

preBisubstitution ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Presubstitution ground ->
    DolanTypeCheckM ground (SolverBisubstitution ground)
preBisubstitution (cleanPresubstitution -> MkPresubstitution (oldvar :: _ oldtv) (newvar :: _ newtv) msta mstb) = do
    mftwa :: (_ -> TypeResult ground (DolanShimWit ground 'Negative oldtv)) -> TypeResult ground (DolanShimWit ground 'Positive oldtv) <-
        case msta of
            FailureResult err -> return $ \_ -> FailureResult err
            SuccessResult (MkSubShimWit (ta :: _ a) conva) ->
                if variableOccursIn oldvar ta
                    then do
                        MkSomeTypeVarT recvar <- renamerGenerateFreeTypeVarT
                        assignTypeVarT @(JoinType newtv a) recvar $ let
                            recwit = mapShimWit conva $ varDolanShimWit recvar
                            in return $ \mtwb -> do
                                twb <- mtwb recwit
                                return
                                    $ mapShimWit conva
                                    $ shimWitToDolan
                                    $ recursiveDolanShimWit recvar
                                    $ joinMeetShimWit
                                        (varDolanShimWit newvar)
                                        (bothBisubstitute oldvar recwit twb (mkShimWit ta))
                    else return $ \_ -> return $ MkShimWit (ConsDolanType (VarDolanSingularType newvar) ta) conva
    mftwb :: (_ -> TypeResult ground (DolanShimWit ground 'Positive oldtv)) -> TypeResult ground (DolanShimWit ground 'Negative oldtv) <-
        case mstb of
            FailureResult err -> return $ \_ -> FailureResult err
            SuccessResult (MkSubShimWit (tb :: _ b) convb) ->
                if variableOccursIn oldvar tb
                    then do
                        MkSomeTypeVarT recvar <- renamerGenerateFreeTypeVarT
                        assignTypeVarT @(MeetType newtv b) recvar $ let
                            recwit = mapShimWit convb $ varDolanShimWit recvar
                            in return $ \mtwa -> do
                                twa <- mtwa recwit
                                return
                                    $ mapShimWit convb
                                    $ shimWitToDolan
                                    $ recursiveDolanShimWit recvar
                                    $ joinMeetShimWit
                                        (varDolanShimWit newvar)
                                        (bothBisubstitute oldvar recwit twa (mkShimWit tb))
                    else return $ \_ -> return $ MkShimWit (ConsDolanType (VarDolanSingularType newvar) tb) convb
    let
        mtwa :: TypeResult ground (DolanShimWit ground 'Positive oldtv)
        mtwa = mftwa $ \rv -> mftwb $ \_ -> return rv
        mtwb :: TypeResult ground (DolanShimWit ground 'Negative oldtv)
        mtwb = mftwb $ \rv -> mftwa $ \_ -> return rv
    return $ MkBisubstitution oldvar mtwa mtwb

substBisub ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    SolverBisubstitution ground ->
    Presubstitution ground ->
    Presubstitution ground
substBisub bisub (MkPresubstitution oldvar newvar msta mstb) = let
    msta' = do
        MkSubShimWit ta conva <- msta
        MkShimWit ta' sconva <- bisubstituteType bisub ta
        return $ MkSubShimWit ta' $ iPolarPair id sconva . conva
    mstb' = do
        MkSubShimWit tb convb <- mstb
        MkShimWit tb' sconvb <- bisubstituteType bisub tb
        return $ MkSubShimWit tb' $ iPolarPair id sconvb . convb
    in MkPresubstitution oldvar newvar msta' mstb'

presubstitute ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Presubstitution ground ->
    Presubstitution ground ->
    DolanTypeCheckM ground (Presubstitution ground)
presubstitute ps pst = do
    bisub <- preBisubstitution ps
    return $ substBisub bisub pst

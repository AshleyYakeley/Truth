module Language.Expression.Dolan.Solver.Crumble.Presubstitution
    ( Presubstitution
    , assignPresubstitution
    , presubstitute
    , preBisubstitution
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

-- | For debugging.
genNewNameINTERNAL :: Bool
genNewNameINTERNAL = True

assignPresubstitution ::
       forall (ground :: GroundTypeKind) v a b. IsDolanGroundType ground
    => TypeVarT v
    -> DolanType ground 'Positive a
    -> DolanType ground 'Negative b
    -> DolanTypeCheckM ground ( Presubstitution ground
                              , DolanShim ground a b -> (DolanShim ground a v, DolanShim ground v b))
assignPresubstitution oldvar ta tb = do
    MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
        if genNewNameINTERNAL
            then renamerGenerateFreeTypeVarT
            else return $ MkSomeTypeVarT oldvar
    assignTypeVarT @(MeetType (JoinType newtv a) b) oldvar $ do
        return
            (MkPresubstitution oldvar newvar ta tb meet1 (iMeetPair join1 id), \convab -> (meetf join2 convab, meet2))

data Presubstitution (ground :: GroundTypeKind) where
    MkPresubstitution
        :: forall (ground :: GroundTypeKind) oldtv newtv a b.
           TypeVarT oldtv
        -> TypeVarT newtv
        -> DolanType ground 'Positive a
        -> DolanType ground 'Negative b
        -> DolanShim ground oldtv (JoinType newtv a)
        -> DolanShim ground (MeetType newtv b) oldtv
        -> Presubstitution ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (Presubstitution ground) where
    show (MkPresubstitution oldvar newvar ta tb _ _) =
        "{" <>
        show oldvar <>
        "+ => " <>
        show newvar <>
        " | " <> showDolanType ta <> "; " <> show oldvar <> "- => " <> show newvar <> " & " <> showDolanType tb <> "}"

isCleanType ::
       forall (ground :: GroundTypeKind) polarity oldtv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT oldtv
    -> DolanType ground polarity a
    -> Bool
isCleanType _ NilDolanType = True
isCleanType var (ConsDolanType (VarDolanSingularType v) _)
    | Just Refl <- testEquality var v = False
isCleanType var (ConsDolanType _ t) = isCleanType var t

toVarType ::
       forall (ground :: GroundTypeKind) polarity oldtv a r. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT oldtv
    -> DolanType ground polarity a
    -> (forall a'.
                DolanType ground polarity a' -> DolanPolarShim ground polarity a (JoinMeetType polarity a' oldtv) -> r)
    -> r
toVarType _ NilDolanType call = call NilDolanType polar1
toVarType var (ConsDolanType (VarDolanSingularType v) t) call
    | Just Refl <- testEquality var v = toVarType var t $ \t' conv -> call t' $ polarF polar2 conv
toVarType var (ConsDolanType st t) call =
    toVarType var t $ \t' conv -> call (ConsDolanType st t') $ iPolarSwapR . iPolarPair id conv

cleanType ::
       forall (ground :: GroundTypeKind) polarity oldtv newtv a r. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT oldtv
    -> DolanType ground polarity a
    -> DolanPolarShim ground polarity oldtv (JoinMeetType polarity newtv a)
    -> (forall a'.
                DolanType ground polarity a' -> DolanPolarShim ground polarity oldtv (JoinMeetType polarity newtv a') -> r)
    -> r
cleanType var t conv call =
    if isCleanType var t
        then call t conv
        else toVarType var t $ \t' vconv -> let
                 rconv = polarF polar1 (polarF polar2 (lazyPolarShim rconv) . vconv) . conv
                 in call t' rconv

cleanPresubstitution ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Presubstitution ground
    -> Presubstitution ground
cleanPresubstitution (MkPresubstitution oldvar newvar ta tb conva convb) =
    cleanType oldvar ta (MkPolarShim conva) $ \ta' (MkPolarShim conva') ->
        cleanType oldvar tb (MkPolarShim convb) $ \tb' (MkPolarShim convb') ->
            MkPresubstitution oldvar newvar ta' tb' conva' convb'

preBisubstitution ::
       forall (ground :: GroundTypeKind) m. (IsDolanGroundType ground, Applicative m)
    => Presubstitution ground
    -> DolanTypeCheckM ground (Bisubstitution ground (DolanShim ground) m)
preBisubstitution (cleanPresubstitution -> MkPresubstitution (oldvar :: _ oldtv) (newvar :: _ newtv) (ta :: _ a) (tb :: _ b) conva convb) = do
    ftwa <-
        if variableOccursIn oldvar ta
            then do
                MkSomeTypeVarT recvar <- renamerGenerateFreeTypeVarT
                assignTypeVarT @(JoinType newtv a) recvar $ let
                    recwit = mapShimWit (MkPolarShim conva) $ varDolanShimWit recvar
                    in return $ \twb ->
                           mapShimWit (MkPolarShim conva) $
                           shimWitToDolan $
                           recursiveDolanShimWit recvar $
                           joinMeetShimWit
                               (varDolanShimWit newvar)
                               (bothBisubstitute oldvar recwit (twb recwit) (mkShimWit ta))
            else return $ \_ -> MkShimWit (ConsDolanType (VarDolanSingularType newvar) ta) $ MkPolarShim conva
    ftwb <-
        if variableOccursIn oldvar tb
            then do
                MkSomeTypeVarT recvar <- renamerGenerateFreeTypeVarT
                assignTypeVarT @(MeetType newtv b) recvar $ let
                    recwit = mapShimWit (MkPolarShim convb) $ varDolanShimWit recvar
                    in return $ \twa ->
                           mapShimWit (MkPolarShim convb) $
                           shimWitToDolan $
                           recursiveDolanShimWit recvar $
                           joinMeetShimWit
                               (varDolanShimWit newvar)
                               (bothBisubstitute oldvar recwit (twa recwit) (mkShimWit tb))
            else return $ \_ -> MkShimWit (ConsDolanType (VarDolanSingularType newvar) tb) $ MkPolarShim convb
    let
        twa :: DolanShimWit ground 'Positive oldtv
        twa = ftwa $ \rv -> ftwb $ \_ -> rv
        twb :: DolanShimWit ground 'Negative oldtv
        twb = ftwb $ \rv -> ftwa $ \_ -> rv
    return $ MkBisubstitution oldvar (pure twa) (pure twb)

substBisub ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Bisubstitution ground (DolanShim ground) Identity
    -> Presubstitution ground
    -> Presubstitution ground
substBisub bisub (MkPresubstitution oldvar newvar ta tb conva convb) =
    runIdentity $ do
        MkShimWit sta (MkPolarShim sconva) <- bisubstituteType bisub ta
        MkShimWit stb (MkPolarShim sconvb) <- bisubstituteType bisub tb
        let
            conva' = iJoinPair id sconva . conva
            convb' = convb . iMeetPair id sconvb
        return $ MkPresubstitution oldvar newvar sta stb conva' convb'

presubstitute ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => Presubstitution ground
    -> Presubstitution ground
    -> DolanTypeCheckM ground (Presubstitution ground)
presubstitute ps pst = do
    bisub <- preBisubstitution ps
    return $ substBisub bisub pst
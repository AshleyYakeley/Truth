module Pinafore.Language.TypeSystem.Bisubstitute where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Shim
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

type PinaforeBisubstitutionM m = Bisubstitution (PinaforeShim Type) m PinaforeType

type PinaforeBisubstitution = PinaforeBisubstitutionM (PinaforeTypeCheck)

bisubstitutePositiveSingularType ::
       Monad m => PinaforeBisubstitutionM m -> PinaforeSingularType 'Positive t -> m (PinaforeTypeShimWit 'Positive t)
bisubstitutePositiveSingularType (MkBisubstitution n tp _) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tp
bisubstitutePositiveSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeShimWit $ mkShimWit t
bisubstitutePositiveSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeVarianceType gt
    in do
           MkShimWit args' conv <-
               mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVarianceMap gt) args
           return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

bisubstituteNegativeSingularType ::
       Monad m => PinaforeBisubstitutionM m -> PinaforeSingularType 'Negative t -> m (PinaforeTypeShimWit 'Negative t)
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeShimWit $ mkShimWit t
bisubstituteNegativeSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeVarianceType gt
    in do
           MkShimWit args' conv <-
               mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVarianceMap gt) args
           return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

bisubstitutePositiveType ::
       Monad m => PinaforeBisubstitutionM m -> PinaforeType 'Positive t -> m (PinaforeTypeShimWit 'Positive t)
bisubstitutePositiveType _ NilPinaforeType = return $ mkShimWit NilPinaforeType
bisubstitutePositiveType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstitutePositiveSingularType bisub ta
    tfb <- bisubstitutePositiveType bisub tb
    return $ joinMeetPinaforeShimWit tfa tfb

bisubstituteNegativeType ::
       Monad m => PinaforeBisubstitutionM m -> PinaforeType 'Negative t -> m (PinaforeTypeShimWit 'Negative t)
bisubstituteNegativeType _ NilPinaforeType = return $ mkShimWit NilPinaforeType
bisubstituteNegativeType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstituteNegativeSingularType bisub ta
    tfb <- bisubstituteNegativeType bisub tb
    return $ joinMeetPinaforeShimWit tfa tfb

bisubstituteType ::
       forall m polarity t. (Monad m, Is PolarityType polarity)
    => PinaforeBisubstitutionM m
    -> PinaforeType polarity t
    -> m (PinaforeTypeShimWit polarity t)
bisubstituteType =
    case polarityType @polarity of
        PositiveType -> bisubstitutePositiveType
        NegativeType -> bisubstituteNegativeType

bisubstitutesType ::
       forall m polarity t. (Monad m, Is PolarityType polarity)
    => [PinaforeBisubstitutionM m]
    -> PinaforeType polarity t
    -> m (PinaforeTypeShimWit polarity t)
bisubstitutesType [] t = return $ mkShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall m a. (Monad m, PShimWitMappable (PinaforeShim Type) PinaforeType a)
    => [PinaforeBisubstitutionM m]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutes subs expr'

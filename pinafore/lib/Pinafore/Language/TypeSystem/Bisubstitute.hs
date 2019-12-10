module Pinafore.Language.TypeSystem.Bisubstitute where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

type PinaforeBisubstitutionM m baseupdate = Bisubstitution m (PinaforeType baseupdate)

type PinaforeBisubstitution baseupdate = PinaforeBisubstitutionM (PinaforeTypeCheck baseupdate) baseupdate

bisubstitutePositiveSingularType ::
       Monad m
    => PinaforeBisubstitutionM m baseupdate
    -> PinaforeSingularType baseupdate 'Positive t
    -> m (PinaforeShimWit baseupdate 'Positive t)
bisubstitutePositiveSingularType (MkBisubstitution n tp _) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tp
bisubstitutePositiveSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeShimWit $ mkPJMShimWit t
bisubstitutePositiveSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeVarianceType gt
    in do
           MkShimWit args' conv <-
               mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVarianceMap gt) args
           return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

bisubstituteNegativeSingularType ::
       Monad m
    => PinaforeBisubstitutionM m baseupdate
    -> PinaforeSingularType baseupdate 'Negative t
    -> m (PinaforeShimWit baseupdate 'Negative t)
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeShimWit $ mkPJMShimWit t
bisubstituteNegativeSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeVarianceType gt
    in do
           MkShimWit args' conv <-
               mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVarianceMap gt) args
           return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

bisubstitutePositiveType ::
       Monad m
    => PinaforeBisubstitutionM m baseupdate
    -> PinaforeType baseupdate 'Positive t
    -> m (PinaforeShimWit baseupdate 'Positive t)
bisubstitutePositiveType _ NilPinaforeType = return $ mkPJMShimWit NilPinaforeType
bisubstitutePositiveType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstitutePositiveSingularType bisub ta
    tfb <- bisubstitutePositiveType bisub tb
    return $ joinPinaforeShimWit tfa tfb

bisubstituteNegativeType ::
       Monad m
    => PinaforeBisubstitutionM m baseupdate
    -> PinaforeType baseupdate 'Negative t
    -> m (PinaforeShimWit baseupdate 'Negative t)
bisubstituteNegativeType _ NilPinaforeType = return $ mkPJMShimWit NilPinaforeType
bisubstituteNegativeType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstituteNegativeSingularType bisub ta
    tfb <- bisubstituteNegativeType bisub tb
    return $ meetPinaforeShimWit tfa tfb

bisubstituteType ::
       forall baseupdate m polarity t. (Monad m, Is PolarityType polarity)
    => PinaforeBisubstitutionM m baseupdate
    -> PinaforeType baseupdate polarity t
    -> m (PinaforeShimWit baseupdate polarity t)
bisubstituteType =
    case representative @_ @_ @polarity of
        PositiveType -> bisubstitutePositiveType
        NegativeType -> bisubstituteNegativeType

bisubstitutesType ::
       forall baseupdate m polarity t. (Monad m, Is PolarityType polarity)
    => [PinaforeBisubstitutionM m baseupdate]
    -> PinaforeType baseupdate polarity t
    -> m (PinaforeShimWit baseupdate polarity t)
bisubstitutesType [] t = return $ mkPJMShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall baseupdate m a. (Monad m, PShimWitMappable PinaforeShim (PinaforeType baseupdate) a)
    => [PinaforeBisubstitutionM m baseupdate]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutes subs expr'

module Pinafore.Language.Type.Bisubstitute where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

type PinaforeBisubstitutionM m baseedit = Bisubstitution m (PinaforeType baseedit)

type PinaforeBisubstitution baseedit = PinaforeBisubstitutionM (PinaforeTypeCheck baseedit) baseedit

bisubstitutePositiveSingularType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeSingularType baseedit 'Positive t
    -> m (PinaforeShimWit baseedit 'Positive t)
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
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeSingularType baseedit 'Negative t
    -> m (PinaforeShimWit baseedit 'Negative t)
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
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit 'Positive t
    -> m (PinaforeShimWit baseedit 'Positive t)
bisubstitutePositiveType _ NilPinaforeType = return $ mkPJMShimWit NilPinaforeType
bisubstitutePositiveType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstitutePositiveSingularType bisub ta
    tfb <- bisubstitutePositiveType bisub tb
    return $ joinPinaforeShimWit tfa tfb

bisubstituteNegativeType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit 'Negative t
    -> m (PinaforeShimWit baseedit 'Negative t)
bisubstituteNegativeType _ NilPinaforeType = return $ mkPJMShimWit NilPinaforeType
bisubstituteNegativeType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstituteNegativeSingularType bisub ta
    tfb <- bisubstituteNegativeType bisub tb
    return $ meetPinaforeShimWit tfa tfb

bisubstituteType ::
       forall baseedit m polarity t. (Monad m, Is PolarityType polarity)
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit polarity t
    -> m (PinaforeShimWit baseedit polarity t)
bisubstituteType =
    case representative @_ @_ @polarity of
        PositiveType -> bisubstitutePositiveType
        NegativeType -> bisubstituteNegativeType

bisubstitutesType ::
       forall baseedit m polarity t. (Monad m, Is PolarityType polarity)
    => [PinaforeBisubstitutionM m baseedit]
    -> PinaforeType baseedit polarity t
    -> m (PinaforeShimWit baseedit polarity t)
bisubstitutesType [] t = return $ mkPJMShimWit t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainShimWitM (bisubstitutesType subs) tf

bisubstitutes ::
       forall baseedit m a. (Monad m, PShimWitMappable PinaforeShim (PinaforeType baseedit) a)
    => [PinaforeBisubstitutionM m baseedit]
    -> a
    -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapPShimWitsM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutes subs expr'

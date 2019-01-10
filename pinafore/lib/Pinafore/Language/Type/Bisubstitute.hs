module Pinafore.Language.Type.Bisubstitute where

import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

type PinaforeBisubstitutionM m baseedit = Bisubstitution m (PinaforeType baseedit)

type PinaforeBisubstitution baseedit = PinaforeBisubstitutionM (PinaforeTypeCheck baseedit) baseedit

bisubstitutePositiveSingularType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeSingularType baseedit 'PositivePolarity t
    -> m (PinaforeTypeF baseedit 'PositivePolarity t)
bisubstitutePositiveSingularType (MkBisubstitution n tp _) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tp
bisubstitutePositiveSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeTypeF $ mkTypeF t
bisubstitutePositiveSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in do
           MkTypeF args' conv <- mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args
           return $ singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

bisubstituteNegativeSingularType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeSingularType baseedit 'NegativePolarity t
    -> m (PinaforeTypeF baseedit 'NegativePolarity t)
bisubstituteNegativeSingularType (MkBisubstitution n _ tq) (VarPinaforeSingularType n')
    | Just Refl <- testEquality n n' = tq
bisubstituteNegativeSingularType _ t@(VarPinaforeSingularType _) = return $ singlePinaforeTypeF $ mkTypeF t
bisubstituteNegativeSingularType bisub (GroundPinaforeSingularType gt args) = let
    dvt = pinaforeGroundTypeKind gt
    in do
           MkTypeF args' conv <- mapDolanArgumentsM (bisubstituteType bisub) dvt (pinaforeGroundTypeVary gt) args
           return $ singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt args') conv

bisubstitutePositiveType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit 'PositivePolarity t
    -> m (PinaforeTypeF baseedit 'PositivePolarity t)
bisubstitutePositiveType _ NilPinaforeType = return $ mkTypeF NilPinaforeType
bisubstitutePositiveType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstitutePositiveSingularType bisub ta
    tfb <- bisubstitutePositiveType bisub tb
    return $ joinPinaforeTypeF tfa tfb

bisubstituteNegativeType ::
       Monad m
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit 'NegativePolarity t
    -> m (PinaforeTypeF baseedit 'NegativePolarity t)
bisubstituteNegativeType _ NilPinaforeType = return $ mkTypeF NilPinaforeType
bisubstituteNegativeType bisub (ConsPinaforeType ta tb) = do
    tfa <- bisubstituteNegativeSingularType bisub ta
    tfb <- bisubstituteNegativeType bisub tb
    return $ meetPinaforeTypeF tfa tfb

bisubstituteType ::
       forall baseedit m polarity t. (Monad m, IsTypePolarity polarity)
    => PinaforeBisubstitutionM m baseedit
    -> PinaforeType baseedit polarity t
    -> m (PinaforeTypeF baseedit polarity t)
bisubstituteType =
    case whichTypePolarity @polarity of
        Left Refl -> bisubstitutePositiveType
        Right Refl -> bisubstituteNegativeType

bisubstitutesType ::
       forall baseedit m polarity t. (Monad m, IsTypePolarity polarity)
    => [PinaforeBisubstitutionM m baseedit]
    -> PinaforeType baseedit polarity t
    -> m (PinaforeTypeF baseedit polarity t)
bisubstitutesType [] t = return $ mkTypeF t
bisubstitutesType (sub:subs) t = do
    tf <- bisubstituteType sub t
    chainTypeFM (bisubstitutesType subs) tf

bisubstitutes :: (Monad m, TypeMappable (PinaforeType baseedit) a) => [PinaforeBisubstitutionM m baseedit] -> a -> m a
bisubstitutes [] expr = return $ expr
bisubstitutes (sub:subs) expr = do
    expr' <- mapTypesM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutes subs expr'
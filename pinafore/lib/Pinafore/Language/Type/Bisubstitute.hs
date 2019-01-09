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

bisubstituteAllPositiveType ::
       Monad m
    => [PinaforeBisubstitutionM m baseedit]
    -> PinaforeType baseedit 'PositivePolarity t
    -> m (PinaforeTypeF baseedit 'PositivePolarity t)
bisubstituteAllPositiveType [] t = return $ mkTypeF t
bisubstituteAllPositiveType (sub:subs) t = do
    MkTypeF t' conv <- bisubstitutePositiveType sub t
    tf <- bisubstituteAllPositiveType subs t'
    return $ contramap conv tf

bisubstituteAllNegativeType ::
       Monad m
    => [PinaforeBisubstitutionM m baseedit]
    -> PinaforeType baseedit 'NegativePolarity t
    -> m (PinaforeTypeF baseedit 'NegativePolarity t)
bisubstituteAllNegativeType [] t = return $ mkTypeF t
bisubstituteAllNegativeType (sub:subs) t = do
    MkTypeF t' conv <- bisubstituteNegativeType sub t
    tf <- bisubstituteAllNegativeType subs t'
    return $ fmap conv tf

bisubstitutesSealedExpression ::
       Monad m => [PinaforeBisubstitutionM m baseedit] -> PinaforeExpression baseedit -> m (PinaforeExpression baseedit)
bisubstitutesSealedExpression [] expr = return $ expr
bisubstitutesSealedExpression (sub:subs) expr = do
    expr' <- mapSealedExpressionTypesM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutesSealedExpression subs expr'

bisubstitutesSealedPattern ::
       Monad m => [PinaforeBisubstitutionM m baseedit] -> PinaforePattern baseedit -> m (PinaforePattern baseedit)
bisubstitutesSealedPattern [] expr = return $ expr
bisubstitutesSealedPattern (sub:subs) expr = do
    expr' <- mapSealedPatternTypesM (bisubstitutePositiveType sub) (bisubstituteNegativeType sub) expr
    bisubstitutesSealedPattern subs expr'

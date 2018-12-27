module Pinafore.Language.Type.Bisubstitute where

import Language.Expression.Dolan
import Pinafore.Language.GroundType
import Pinafore.Language.Type.Type
import Shapes

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

bisubstitutesType ::
       forall baseedit polarity t. IsTypePolarity polarity
    => [PinaforeBisubstitution baseedit]
    -> PinaforeType baseedit polarity t
    -> PinaforeTypeF baseedit polarity t
bisubstitutesType [] t = mkTypeF t
bisubstitutesType (sub:subs) t = chainTypeF (bisubstitutesType subs) $ bisubstituteType sub t

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

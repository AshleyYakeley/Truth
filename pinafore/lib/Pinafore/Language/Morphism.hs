module Pinafore.Language.Morphism where

import Language.Expression.Dolan
import Pinafore.Morphism
import Shapes

data PinaforeMorphism baseedit pqa pqb =
    forall a b. MkPinaforeMorphism (TypeRange a pqa)
                                   (TypeRange b pqb)
                                   (PinaforeLensMorphism baseedit a b)

instance IsoMapTypeRange (PinaforeMorphism baseedit pqa)

instance MapTypeRange (PinaforeMorphism baseedit pqa) where
    mapTypeRange f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism ra (mapTypeRange f rb) m

instance IsoMapTypeRange' (PinaforeMorphism baseedit)

instance MapTypeRange' (PinaforeMorphism baseedit) where
    mapTypeRange' f (MkPinaforeMorphism ra rb m) = MkPinaforeMorphism (mapTypeRange f ra) rb m

instance HasDolanVary '[ 'Rangevariance, 'Rangevariance] (PinaforeMorphism baseedit) where
    dolanVary =
        ConsDolanKindVary
            (mkRangevary $ \mapr ->
                 MkNestedMorphism $ \(MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism (mapr ra) rb lm) $
        ConsDolanKindVary (mkRangevary $ \mapr (MkPinaforeMorphism ra rb lm) -> MkPinaforeMorphism ra (mapr rb) lm) $
        NilDolanKindVary

pinaforeMorphismLens :: PinaforeMorphism baseedit '( a, a) '( b, b) -> PinaforeLensMorphism baseedit a b
pinaforeMorphismLens (MkPinaforeMorphism tra trb lm) =
    (bijectionPinaforeLensMorphism $ typeRangeBijection trb) .
    lm . (bijectionPinaforeLensMorphism $ invert $ typeRangeBijection tra)

pinaforeLensMorphism :: PinaforeLensMorphism baseedit a b -> PinaforeMorphism baseedit '( a, a) '( b, b)
pinaforeLensMorphism = MkPinaforeMorphism identityTypeRange identityTypeRange

pinaforeMorphismFunction ::
       PinaforeMorphism baseedit '( a, TopType) '( BottomType, b) -> PinaforeFunctionMorphism baseedit a b
pinaforeMorphismFunction (MkPinaforeMorphism tra trb pm) =
    proc a -> do
        tb <- lensFunctionMorphism pm -< typeRangeContra tra a
        returnA -< typeRangeCo trb tb

identityPinaforeMorphism :: PinaforeMorphism baseedit '( t, t) '( t, t)
identityPinaforeMorphism = pinaforeLensMorphism id

composePinaforeMorphism ::
       PinaforeMorphism baseedit '( b, b) '( c, c)
    -> PinaforeMorphism baseedit '( a, a) '( b, b)
    -> PinaforeMorphism baseedit '( a, a) '( c, c)
composePinaforeMorphism mbc mab = pinaforeLensMorphism $ (pinaforeMorphismLens mbc) . (pinaforeMorphismLens mab)

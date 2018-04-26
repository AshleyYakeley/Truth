module Pinafore.Query.Order where

import Data.Time
import Pinafore.Morphism
import Pinafore.Number
import Pinafore.PredicateMorphism
import Pinafore.Query.Types
import Pinafore.Table
import Shapes
import Truth.Core

data QOrder baseedit =
    forall t. MkQOrder (QImLiteralMorphism baseedit t)
                       (t -> t -> Ordering)

instance Semigroup (QOrder baseedit) where
    MkQOrder fa oa <> MkQOrder fb ob =
        MkQOrder (liftA2 (liftA2 (,)) fa fb) $ \(a1, b1) (a2, b2) ->
            case oa a1 a2 of
                EQ -> ob b1 b2
                cmp -> cmp

instance Monoid (QOrder baseedit) where
    mempty = MkQOrder (pure $ pure ()) $ compare @()
    mappend = (<>)

alphabetical ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QOrder baseedit
alphabetical = MkQOrder (lensFunctionMorphism literalPinaforeLensMorphism) $ compare @Text

numerical ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QOrder baseedit
numerical = MkQOrder (lensFunctionMorphism literalPinaforeLensMorphism) $ compare @Number

chronological ::
       forall baseedit. HasPinaforeTableEdit baseedit
    => QOrder baseedit
chronological = MkQOrder (lensFunctionMorphism literalPinaforeLensMorphism) $ compare @UTCTime

orderon :: forall baseedit. (QImPointMorphism baseedit) -> QOrder baseedit -> QOrder baseedit
orderon f (MkQOrder (ef :: QImLiteralMorphism baseedit t) o) = let
    ef' :: QImLiteralMorphism baseedit t
    ef' = arr (\mmt -> mmt >>= id) . cfmap ef . f
    in MkQOrder ef' o

orders :: forall baseedit. [QOrder baseedit] -> QOrder baseedit
orders = mconcat

rev :: forall baseedit. QOrder baseedit -> QOrder baseedit
rev (MkQOrder ef o) = MkQOrder ef $ \a b -> o b a

qOrderSet :: forall baseedit. QOrder baseedit -> QImSet baseedit -> PinaforeFunctionValue baseedit [Point]
qOrderSet (MkQOrder (ofunc :: QImLiteralMorphism baseedit t) oord) pset = let
    cmp :: (Point, Maybe t) -> (Point, Maybe t) -> Ordering
    cmp (_, Just t1) (_, Just t2) = oord t1 t2
    cmp (_, Nothing) (_, Just _) = GT
    cmp (_, Just _) (_, Nothing) = LT
    cmp (_, Nothing) (_, Nothing) = EQ
    ofuncpair :: PinaforeFunctionMorphism baseedit Point (Point, Maybe t)
    ofuncpair =
        proc point -> do
            t <- ofunc -< point
            returnA -< (point, t)
    upairs :: QImLiteralSet baseedit (Point, Maybe t)
    upairs = applyPinaforeFunction (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (Point, Maybe t) -> [Point]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    in unWholeEditFunction $ fmap sortpoints $ MkWholeEditFunction upairs

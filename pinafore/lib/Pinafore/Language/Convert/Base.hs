module Pinafore.Language.Convert.Base
    ( HasQTypeDescription
    , qTypeDescription
    , ToTypeF(..)
    , FromTypeF(..)
    , toValue
    , ToPinaforeType
    , FromPinaforeType
    , literalInstances
    ) where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Morphism
import Pinafore.Language.NamedEntity
import Pinafore.Language.Order
import Pinafore.Language.Reference
import Pinafore.Language.Set
import Pinafore.Language.Type
import Shapes
import Truth.Core

type HasQTypeDescription baseedit = ToPinaforeType baseedit

qTypeDescription ::
       forall baseedit t. HasQTypeDescription baseedit t
    => Text
qTypeDescription =
    case toTypeF @(PinaforeType baseedit) @t of
        MkTypeF w _ -> pack $ show w

class ToTypeF wit t where
    toTypeF :: TypeF wit 'PositivePolarity t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue (wit 'PositivePolarity)
toValue t =
    case toTypeF @wit @t of
        MkTypeF tt conv -> MkAnyValue tt $ conv t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'NegativePolarity t

type ToPinaforeType baseedit = ToTypeF (PinaforeType baseedit)

type FromPinaforeType baseedit = FromTypeF (PinaforeType baseedit)

unToWithTypeF ::
       (FromTypeF tw pa, ToTypeF tw qa)
    => (forall pt qt. TypeRangeWitness tw 'PositivePolarity '( pt, qt) -> WithRange (->) '( pa, qa) '( pt, qt) -> r)
    -> r
unToWithTypeF cont =
    unTypeF fromTypeF $ \tp convp ->
        unTypeF toTypeF $ \tq convq -> cont (MkTypeRangeWitness tp tq) (MkWithRange convp convq)

unFromWithTypeF ::
       (ToTypeF tw pa, FromTypeF tw qa)
    => (forall pt qt. TypeRangeWitness tw 'NegativePolarity '( pt, qt) -> WithRange (->) '( pt, qt) '( pa, qa) -> r)
    -> r
unFromWithTypeF cont =
    unTypeF toTypeF $ \tp convp ->
        unTypeF fromTypeF $ \tq convq -> cont (MkTypeRangeWitness tp tq) (MkWithRange convp convq)

-- top, bottom, join, meet
instance ToTypeF (PinaforeType baseedit) BottomType where
    toTypeF = mkTypeF NilPinaforeType

instance FromTypeF (PinaforeType baseedit) TopType where
    fromTypeF = mkTypeF NilPinaforeType

instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (JoinType a b) where
    toTypeF = joinPinaforeTypeF toTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (MeetType a b) where
    fromTypeF = meetPinaforeTypeF fromTypeF fromTypeF

instance ToTypeF (PinaforeSingularType baseedit) TopType where
    toTypeF = mkTypeF $ GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments

instance ToTypeF (PinaforeType baseedit) TopType where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit) BottomType where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType InvertLimitPinaforeGroundType NilDolanArguments

instance FromTypeF (PinaforeType baseedit) BottomType where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- UVar
instance KnownSymbol name => ToTypeF (PinaforeSingularType baseedit) (UVar name) where
    toTypeF = mkTypeF $ VarPinaforeSingularType MkSymbolWitness

instance KnownSymbol name => ToTypeF (PinaforeType baseedit) (UVar name) where
    toTypeF = singlePinaforeTypeF toTypeF

instance KnownSymbol name => FromTypeF (PinaforeSingularType baseedit) (UVar name) where
    fromTypeF = mkTypeF $ VarPinaforeSingularType MkSymbolWitness

instance KnownSymbol name => FromTypeF (PinaforeType baseedit) (UVar name) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- (,)
instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeSingularType baseedit) (a, b) where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\(a, b) -> (conva a, convb b)) $
                mkTypeF $
                GroundPinaforeSingularType PairPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (a, b) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeSingularType baseedit) (a, b) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\(a, b) -> (conva a, convb b)) $
                mkTypeF $
                GroundPinaforeSingularType PairPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (a, b) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- (->)
instance (FromTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeSingularType baseedit) (a -> b) where
    toTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\ab -> convb . ab . conva) $
                mkTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (a -> b) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (ToTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeSingularType baseedit) (a -> b) where
    fromTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\ab -> convb . ab . conva) $
                mkTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (a -> b) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- []
instance (ToTypeF (PinaforeType baseedit) a) => ToTypeF (PinaforeSingularType baseedit) [a] where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            contramap (fmap conva) $
            mkTypeF $ GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a) => ToTypeF (PinaforeType baseedit) [a] where
    toTypeF = singlePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit) a) => FromTypeF (PinaforeSingularType baseedit) [a] where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            fmap (fmap conva) $
            mkTypeF $ GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (FromTypeF (PinaforeType baseedit) a) => FromTypeF (PinaforeType baseedit) [a] where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeAction
instance baseedit ~ edit => ToTypeF (PinaforeSingularType baseedit) (PinaforeAction edit) where
    toTypeF = mkTypeF $ GroundPinaforeSingularType ActionPinaforeGroundType NilDolanArguments

instance baseedit ~ edit => ToTypeF (PinaforeType baseedit) (PinaforeAction edit) where
    toTypeF = singlePinaforeTypeF toTypeF

instance baseedit ~ edit => FromTypeF (PinaforeSingularType baseedit) (PinaforeAction edit) where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType ActionPinaforeGroundType NilDolanArguments

instance baseedit ~ edit => FromTypeF (PinaforeType baseedit) (PinaforeAction edit) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- View
instance (baseedit ~ edit, seledit ~ ConstEdit Entity) => ToTypeF (PinaforeType baseedit) (View seledit edit ()) where
    toTypeF = contramap pinaforeLiftView toTypeF

instance (baseedit ~ edit, seledit ~ ConstEdit Entity) => FromTypeF (PinaforeType baseedit) (View seledit edit ()) where
    fromTypeF =
        fmap
            (\(MkComposeM v :: PinaforeAction edit) -> do
                 _ <- v -- ignore failure
                 return ())
            fromTypeF

-- PinaforeOrder
instance (baseedit ~ edit, FromTypeF (PinaforeType edit) a) =>
             ToTypeF (PinaforeSingularType baseedit) (PinaforeOrder edit a) where
    toTypeF =
        unTypeF fromTypeF $ \ta conv ->
            contramap (contramap conv) $
            mkTypeF $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType edit) a) => ToTypeF (PinaforeType baseedit) (PinaforeOrder edit a) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit) a) =>
             FromTypeF (PinaforeSingularType baseedit) (PinaforeOrder edit a) where
    fromTypeF =
        unTypeF toTypeF $ \ta conv ->
            fmap (contramap conv) $
            mkTypeF $ GroundPinaforeSingularType OrderPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType edit) a) => FromTypeF (PinaforeType baseedit) (PinaforeOrder edit a) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- UISpec
instance (baseedit ~ edit, seledit ~ ConstEdit Entity) => ToTypeF (PinaforeSingularType baseedit) (UISpec seledit edit) where
    toTypeF = mkTypeF $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance (baseedit ~ edit, seledit ~ ConstEdit Entity) => ToTypeF (PinaforeType baseedit) (UISpec seledit edit) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, seledit ~ ConstEdit Entity) =>
             FromTypeF (PinaforeSingularType baseedit) (UISpec seledit edit) where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType UserInterfacePinaforeGroundType NilDolanArguments

instance (baseedit ~ edit, seledit ~ ConstEdit Entity) => FromTypeF (PinaforeType baseedit) (UISpec seledit edit) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeReference
instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit) p, ToTypeF (PinaforeType baseedit) q) =>
             ToTypeF (PinaforeSingularType baseedit) (PinaforeReference edit '( p, q)) where
    toTypeF =
        unToWithTypeF $ \tpq conv ->
            contramap (mapTypeRange conv) $
            mkTypeF $ GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit) p, ToTypeF (PinaforeType baseedit) q) =>
             ToTypeF (PinaforeType baseedit) (PinaforeReference edit '( p, q)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit) p, FromTypeF (PinaforeType baseedit) q) =>
             FromTypeF (PinaforeSingularType baseedit) (PinaforeReference edit '( p, q)) where
    fromTypeF =
        unFromWithTypeF $ \tpq conv ->
            fmap (mapTypeRange conv) $
            mkTypeF $ GroundPinaforeSingularType ReferencePinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit) p, FromTypeF (PinaforeType baseedit) q) =>
             FromTypeF (PinaforeType baseedit) (PinaforeReference edit '( p, q)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeLensValue
instance (baseedit ~ edit, FromTypeF (PinaforeType edit) t, ToTypeF (PinaforeType edit) t) =>
             ToTypeF (PinaforeType baseedit) (PinaforeLensValue edit (WholeEdit (Know t))) where
    toTypeF = contramap (MkPinaforeReference identityTypeRange) toTypeF

instance (baseedit ~ edit, FromTypeF (PinaforeType edit) t, ToTypeF (PinaforeType edit) t) =>
             FromTypeF (PinaforeType baseedit) (PinaforeLensValue edit (WholeEdit (Know t))) where
    fromTypeF = fmap unPinaforeReference fromTypeF

-- PinaforeImmutableReference
instance (baseedit ~ edit, FromTypeF (PinaforeType edit) a) =>
             FromTypeF (PinaforeType baseedit) (PinaforeImmutableReference baseedit a) where
    fromTypeF = fmap pinaforeReferenceToImmutable fromTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit) a) =>
             ToTypeF (PinaforeType baseedit) (PinaforeImmutableReference baseedit a) where
    toTypeF = contramap pinaforeImmutableToReference toTypeF

-- PinaforeFunctionValue
instance (baseedit ~ edit, FromTypeF (PinaforeType edit) t) =>
             FromTypeF (PinaforeType baseedit) (PinaforeFunctionValue edit (Know t)) where
    fromTypeF = fmap pinaforeReferenceToFunction fromTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType edit) t) =>
             ToTypeF (PinaforeType baseedit) (PinaforeFunctionValue edit (Know t)) where
    toTypeF = contramap pinaforeFunctionToReference toTypeF

--    LiteralPinaforeReference :: TypeRange t pq -> PinaforeLensValue baseedit (WholeEdit (Maybe t)) -> PinaforeReference baseedit pq
{-
instance baseedit ~ edit => ToTypeF (PinaforeType baseedit) (PinaforeLensValue baseedit (WholeEdit Point)) where
    toTypeF = contramap entityLensValuePinaforeReference toTypeF
-}
{-
literalLensValuePinaforeReference :: AsLiteral t => PinaforeLensValue baseedit (WholeEdit (Maybe t)) -> PinaforeReference baseedit '( t,t)
entityLensValuePinaforeReference :: PinaforeLensValue baseedit (WholeEdit Point) -> PinaforeReference baseedit '( NamedEntity name,NamedEntity name)
-}
-- PinaforeSet
instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit) p, ToTypeF (PinaforeType baseedit) q) =>
             ToTypeF (PinaforeSingularType baseedit) (PinaforeSet edit '( p, q)) where
    toTypeF =
        unToWithTypeF $ \tpq conv ->
            contramap (mapTypeRange conv) $
            mkTypeF $ GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, FromTypeF (PinaforeType baseedit) p, ToTypeF (PinaforeType baseedit) q) =>
             ToTypeF (PinaforeType baseedit) (PinaforeSet edit '( p, q)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit) p, FromTypeF (PinaforeType baseedit) q) =>
             FromTypeF (PinaforeSingularType baseedit) (PinaforeSet edit '( p, q)) where
    fromTypeF =
        unFromWithTypeF $ \tpq conv ->
            fmap (mapTypeRange conv) $
            mkTypeF $ GroundPinaforeSingularType SetPinaforeGroundType $ ConsDolanArguments tpq NilDolanArguments

instance (baseedit ~ edit, ToTypeF (PinaforeType baseedit) p, FromTypeF (PinaforeType baseedit) q) =>
             FromTypeF (PinaforeType baseedit) (PinaforeSet edit '( p, q)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- PinaforeLensValue FiniteSetEdit
instance (baseedit ~ edit, ToTypeF (PinaforeType edit) t, FromTypeF (PinaforeType baseedit) t) =>
             FromTypeF (PinaforeType baseedit) (PinaforeLensValue edit (FiniteSetEdit t)) where
    fromTypeF = fmap unPinaforeSet fromTypeF

instance (baseedit ~ edit, Eq t, ToTypeF (PinaforeType baseedit) t, FromTypeF (PinaforeType baseedit) t) =>
             ToTypeF (PinaforeType baseedit) (PinaforeLensValue edit (FiniteSetEdit t)) where
    toTypeF = contramap (MkPinaforeSet identityTypeRange) toTypeF

-- PinaforeMorphism
instance ( baseedit ~ edit
         , FromTypeF (PinaforeType baseedit) pa
         , ToTypeF (PinaforeType baseedit) qa
         , FromTypeF (PinaforeType baseedit) pb
         , ToTypeF (PinaforeType baseedit) qb
         ) => ToTypeF (PinaforeSingularType baseedit) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toTypeF =
        unToWithTypeF $ \ta conva ->
            unToWithTypeF $ \tb convb ->
                contramap (mapTypeRange' conva . mapTypeRange convb) $
                mkTypeF $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , FromTypeF (PinaforeType baseedit) pa
         , ToTypeF (PinaforeType baseedit) qa
         , FromTypeF (PinaforeType baseedit) pb
         , ToTypeF (PinaforeType baseedit) qb
         ) => ToTypeF (PinaforeType baseedit) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    toTypeF = singlePinaforeTypeF toTypeF

instance ( baseedit ~ edit
         , ToTypeF (PinaforeType baseedit) pa
         , FromTypeF (PinaforeType baseedit) qa
         , ToTypeF (PinaforeType baseedit) pb
         , FromTypeF (PinaforeType baseedit) qb
         ) => FromTypeF (PinaforeSingularType baseedit) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromTypeF =
        unFromWithTypeF $ \ta conva ->
            unFromWithTypeF $ \tb convb ->
                fmap (mapTypeRange' conva . mapTypeRange convb) $
                mkTypeF $
                GroundPinaforeSingularType MorphismPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ( baseedit ~ edit
         , ToTypeF (PinaforeType baseedit) pa
         , FromTypeF (PinaforeType baseedit) qa
         , ToTypeF (PinaforeType baseedit) pb
         , FromTypeF (PinaforeType baseedit) qb
         ) => FromTypeF (PinaforeType baseedit) (PinaforeMorphism edit '( pa, qa) '( pb, qb)) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Entity
instance ToTypeF (PinaforeSingularType baseedit) Entity where
    toTypeF = mkTypeF $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments

instance ToTypeF (PinaforeType baseedit) Entity where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit) Entity where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType EntityPinaforeGroundType NilDolanArguments

instance FromTypeF (PinaforeType baseedit) Entity where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- NamedEntity
instance KnownSymbol name => ToTypeF (PinaforeSingularType baseedit) (NamedEntity name) where
    toTypeF = mkTypeF $ GroundPinaforeSingularType (NamedEntityPinaforeGroundType MkSymbolWitness) NilDolanArguments

instance KnownSymbol name => ToTypeF (PinaforeType baseedit) (NamedEntity name) where
    toTypeF = singlePinaforeTypeF toTypeF

instance KnownSymbol name => FromTypeF (PinaforeSingularType baseedit) (NamedEntity name) where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType (NamedEntityPinaforeGroundType MkSymbolWitness) NilDolanArguments

instance KnownSymbol name => FromTypeF (PinaforeType baseedit) (NamedEntity name) where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Point
instance ToTypeF (PinaforeSingularType baseedit) Point where
    toTypeF = mkTypeF $ GroundPinaforeSingularType PointPinaforeGroundType NilDolanArguments

instance ToTypeF (PinaforeType baseedit) Point where
    toTypeF = singlePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit) Point where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType PointPinaforeGroundType NilDolanArguments

instance FromTypeF (PinaforeType baseedit) Point where
    fromTypeF = singlePinaforeTypeF fromTypeF

-- Literal
literalInstances :: _ -> _
literalInstances t =
    [d|
  
  instance ToTypeF (PinaforeSingularType baseedit) $( t ) where
          toTypeF
            = mkTypeF $
                GroundPinaforeSingularType
                  (LiteralPinaforeGroundType representative)
                  NilDolanArguments
  
  instance ToTypeF (PinaforeType baseedit) $( t ) where
          toTypeF = singlePinaforeTypeF toTypeF
  
  instance FromTypeF (PinaforeSingularType baseedit) $( t ) where
          fromTypeF
            = mkTypeF $
                GroundPinaforeSingularType
                  (LiteralPinaforeGroundType representative)
                  NilDolanArguments
  
  instance FromTypeF (PinaforeType baseedit) $( t ) where
          fromTypeF = singlePinaforeTypeF fromTypeF
  |]

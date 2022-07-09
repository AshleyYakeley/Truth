module Pinafore.Language.Library.Std.Monoid
    ( semigroupSubtypeRelationEntry
    , monoidSubtypeRelationEntry
    , monoidLibEntries
    ) where

import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

-- Semigroup
newtype LangSemigroup a =
    MkLangSemigroup (NonEmpty a)
    deriving (Functor, Semigroup)

instance RepresentationalRole LangSemigroup where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangSemigroup where
    maybeRepresentational = Just Dict

instance HasVariance LangSemigroup where
    type VarianceOf LangSemigroup = 'Covariance

semigroupGroundType :: PinaforeGroundType '[ CoCCRVariance] LangSemigroup
semigroupGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangSemigroup)|]) "Semigroup"

instance HasPinaforeGroundType '[ CoCCRVariance] LangSemigroup where
    pinaforeGroundType = semigroupGroundType

semigroupSubtypeRelationEntry ::
       forall t. (HasPinaforeType 'Negative t, HasPinaforeType 'Positive t, Semigroup t)
    => DocTreeEntry BindDoc
semigroupSubtypeRelationEntry =
    hasSubtypeRelationEntry @(LangSemigroup t) @t "Semigroup relationship" $
    functionToShim "sconcat" $ \(MkLangSemigroup aa) -> sconcat aa

-- Monoid
newtype LangMonoid a =
    MkLangMonoid [a]
    deriving (Functor, Semigroup, Monoid)

instance RepresentationalRole LangMonoid where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational LangMonoid where
    maybeRepresentational = Just Dict

instance HasVariance LangMonoid where
    type VarianceOf LangMonoid = 'Covariance

monoidGroundType :: PinaforeGroundType '[ CoCCRVariance] LangMonoid
monoidGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangMonoid)|]) "Monoid"

instance HasPinaforeGroundType '[ CoCCRVariance] LangMonoid where
    pinaforeGroundType = monoidGroundType

monoidSubtypeRelationEntry ::
       forall t. (HasPinaforeType 'Negative t, HasPinaforeType 'Positive t, Monoid t)
    => DocTreeEntry BindDoc
monoidSubtypeRelationEntry =
    hasSubtypeRelationEntry @(LangMonoid t) @t "Monoidal relationship" $
    functionToShim "mconcat" $ \(MkLangMonoid aa) -> mconcat aa

monoidLibEntries :: [DocTreeEntry BindDoc]
monoidLibEntries =
    [ docTreeEntry
          "Semigroup & Monoid"
          ""
          [ mkTypeEntry "Semigroup" "" $ MkBoundType semigroupGroundType
          , mkTypeEntry "Monoid" "" $ MkBoundType monoidGroundType
          , mkValEntry
                "++"
                "Concatentate two items."
                ((\a b -> MkLangSemigroup $ a :| [b]) :: A -> A -> LangSemigroup A)
          , mkValEntry "empty" "The empty value." (MkLangMonoid [] :: LangMonoid BottomType)
          , mkValEntry "concat" "Concatentate some items." (MkLangMonoid :: [A] -> LangMonoid A)
          , mkValEntry "concat1" "Concatentate some items." (MkLangSemigroup :: NonEmpty A -> LangSemigroup A)
          , hasSubtypeRelationEntry @(LangSemigroup A) @(LangMonoid A) "" $
            functionToShim "semigroupToMonoid" $ \(MkLangSemigroup aa) -> MkLangMonoid $ toList aa
          ]
    ]

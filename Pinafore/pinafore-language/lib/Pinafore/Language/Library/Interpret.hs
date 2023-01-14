module Pinafore.Language.Library.Interpret
    ( Interpret(..)
    , interpretLibSection
    ) where

import Pinafore.Language.Convert.HasType
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

newtype Interpret a = MkInterpret
    { interpret :: a
    }

instance Functor Interpret where
    fmap ab (MkInterpret a) = MkInterpret $ ab a

instance HasVariance Interpret where
    type VarianceOf Interpret = 'Covariance

instance RepresentationalRole Interpret where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational Interpret where
    maybeRepresentational = Just Dict

-- Interpret
interpretGroundType :: QGroundType '[ CoCCRVariance] Interpret
interpretGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Interpret)|]) "Interpret"

instance HasQGroundType '[ CoCCRVariance] Interpret where
    qGroundType = interpretGroundType

interpretLibSection :: BindDocTree context
interpretLibSection =
    headingBDT
        "Interpretation"
        ""
        [ typeBDT "Interpret" "" (MkSomeGroundType interpretGroundType) []
        , valPatBDT "MkInterpret" "" (MkInterpret @A) $ PureFunction $ \(MkInterpret @A x) -> (x, ())
        , namespaceBDT "Interpret" "" $
          [ valBDT "map" "" (fmap :: (A -> B) -> Interpret A -> Interpret B)
          , nameInRootBDT $ valBDT "interpret" "" $ interpret @A
          ]
        ]

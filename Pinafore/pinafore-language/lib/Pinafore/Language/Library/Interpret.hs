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

interpretLibSection :: BindDocStuff context
interpretLibSection =
    headingBDS
        "Interpretation"
        ""
        [ typeBDS "Interpret" "" (MkSomeGroundType interpretGroundType) []
        , valPatBDS "MkInterpret" "" (MkInterpret @A) $ PureFunction $ \(MkInterpret @A x) -> (x, ())
        , namespaceBDS "Interpret" "" $
          functorEntries @_ @Interpret <>
          [ valBDS "map" "" (fmap :: (A -> B) -> Interpret A -> Interpret B)
          , addNameInRootBDS $ valBDS "interpret" "" $ interpret @A
          ]
        ]

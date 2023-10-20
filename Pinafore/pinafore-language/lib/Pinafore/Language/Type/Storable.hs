{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Storable
    ( Storability(..)
    , storabilityProperty
    , entityGroundType
    , getMonoStorableType
    ) where

import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Storable.Type
import Shapes

entityGroundType :: QGroundType '[] Entity
entityGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Entity)|]) "Entity"

instance CovarySubtype QGroundType StorableGroundType where
    dolanToMonoGroundType ::
           forall (dv :: CCRVariances) (t :: CCRVariancesKind dv).
           QGroundType dv t
        -> Maybe (CovaryType dv, StorableGroundType t)
    dolanToMonoGroundType agt = do
        storability <- getGroundProperty storabilityProperty agt
        return $
            ( stbKind storability
            , MkStorableGroundType (qgtFamilyType agt) $ MkSealedStorability (qgtShowType agt) storability)

getMonoStorableType :: QNonpolarType t -> QInterpreter (MonoStorableType t)
getMonoStorableType tm =
    case nonpolarToMonoType tm of
        Just t -> return t
        Nothing -> throw $ InterpretTypeNotEntityError $ exprShow tm

module Pinafore.Language.Type.DynamicSupertype
    ( GreatestDynamicSupertype(..)
    , getGreatestDynamicSupertype
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Base
import Pinafore.Language.Shim
import Pinafore.Language.Type.DynamicEntity
import Pinafore.Language.Type.Entity
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Literal
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.Type.Type
import Shapes

data GreatestDynamicSupertype t =
    forall dt. MkGreatestDynamicSupertype (PinaforeShimWit 'Negative dt)
                                          (PinaforePolyShim Type t dt)
                                          (PinaforePolyShim Type dt (Maybe t))

isomapGDS :: Isomorphism (PinaforePolyShim Type) a b -> GreatestDynamicSupertype a -> GreatestDynamicSupertype b
isomapGDS (MkIsomorphism ab ba) (MkGreatestDynamicSupertype d ad dma) =
    MkGreatestDynamicSupertype d (ad . ba) (applyCoPolyShim cid ab . dma)

class MakeGreatestDynamicSupertype w where
    toNegativeShimWit :: w dt -> PinaforeShimWit 'Negative dt

makeGDS ::
       MakeGreatestDynamicSupertype w
    => w dt
    -> PinaforePolyShim Type t dt
    -> PinaforePolyShim Type dt (Maybe t)
    -> GreatestDynamicSupertype t
makeGDS wt = MkGreatestDynamicSupertype $ toNegativeShimWit wt

codecGDS :: MakeGreatestDynamicSupertype w => w dt -> Codec dt t -> GreatestDynamicSupertype t
codecGDS wt codec = makeGDS wt (functionToShim "subtype" $ encode codec) (functionToShim "supertype" $ decode codec)

instance MakeGreatestDynamicSupertype (PinaforeShimWit 'Negative) where
    toNegativeShimWit wt = wt

instance MakeGreatestDynamicSupertype (PinaforeSingularType 'Negative) where
    toNegativeShimWit wt = singleDolanShimWit $ mkShimWit wt

instance MakeGreatestDynamicSupertype (PinaforeGroundType '[]) where
    toNegativeShimWit wt =
        toNegativeShimWit @(PinaforeSingularType 'Negative) $ GroundDolanSingularType wt NilDolanArguments

instance MakeGreatestDynamicSupertype EntityGroundType where
    toNegativeShimWit wt = toNegativeShimWit $ EntityPinaforeGroundType NilListType wt

instance MakeGreatestDynamicSupertype LiteralType where
    toNegativeShimWit wt = toNegativeShimWit $ LiteralEntityGroundType wt

dynamicEntitySupertype :: DynamicEntityType -> GreatestDynamicSupertype DynamicEntity
dynamicEntitySupertype dt =
    makeGDS TopDynamicEntityGroundType id $
    functionToShim "supertype" $ \e@(MkDynamicEntity t _) ->
        if member t dt
            then Just e
            else Nothing

literalSupertype :: LiteralType t -> Maybe (GreatestDynamicSupertype t)
literalSupertype RationalLiteralType = Just $ codecGDS NumberLiteralType safeRationalNumber
literalSupertype IntegerLiteralType = Just $ codecGDS NumberLiteralType $ integerSafeRational . safeRationalNumber
literalSupertype _ = Nothing

entitySupertype :: EntityGroundType t -> Maybe (GreatestDynamicSupertype t)
entitySupertype (ADynamicEntityGroundType _ dt) = Just $ dynamicEntitySupertype dt
entitySupertype (LiteralEntityGroundType t) = literalSupertype t
entitySupertype _ = Nothing

groundSupertype ::
       forall (dv :: DolanVariance) gt t.
       PinaforeGroundType dv gt
    -> DolanArguments dv PinaforeType gt 'Positive t
    -> Maybe (GreatestDynamicSupertype t)
groundSupertype (EntityPinaforeGroundType NilListType egt) NilDolanArguments = entitySupertype egt
groundSupertype _ _ = Nothing

getGreatestDynamicSupertype :: PinaforeType 'Positive t -> PinaforeSourceInterpreter (GreatestDynamicSupertype t)
getGreatestDynamicSupertype (ConsDolanType (GroundDolanSingularType gt args) NilDolanType)
    | Just ds <- groundSupertype gt args = return $ isomapGDS iJoinR1 ds
getGreatestDynamicSupertype t = do
    t' <- invertType t
    return $ MkGreatestDynamicSupertype t' id $ functionToShim "dynamic-supertype" Just

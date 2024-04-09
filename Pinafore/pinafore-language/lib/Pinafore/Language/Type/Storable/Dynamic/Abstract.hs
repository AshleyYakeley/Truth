module Pinafore.Language.Type.Storable.Dynamic.Abstract
    ( AbstractDynamicEntityFamily(..)
    , abstractDynamicStorableGroundType
    , abstractDynamicStorableFamilyWitness
    ) where

import Data.Shim
import Language.Expression.Dolan
import Pinafore.Language.Name
import Pinafore.Language.Type.DynamicSupertype
import Pinafore.Language.Type.Family
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Storable.Dynamic.Concrete
import Pinafore.Language.Type.Storable.Dynamic.DynamicEntity
import Pinafore.Language.Type.Storable.Dynamic.Entity
import Pinafore.Language.Type.Storable.Dynamic.Storability
import Pinafore.Language.Type.Storable.Type
import Shapes

data AbstractDynamicEntityFamily :: FamilyKind where
    MkAbstractDynamicEntityFamily :: TypeIDType tid -> AbstractDynamicEntityFamily DynamicEntity

instance TestHetEquality AbstractDynamicEntityFamily where
    testHetEquality (MkAbstractDynamicEntityFamily t1) (MkAbstractDynamicEntityFamily t2) = do
        Refl <- testEquality t1 t2
        return HRefl

instance Eq (AbstractDynamicEntityFamily t) where
    f1 == f2 = isJust $ testHetEquality f1 f2

abstractDynamicStorableFamilyWitness :: IOWitness ('MkWitKind AbstractDynamicEntityFamily)
abstractDynamicStorableFamilyWitness = $(iowitness [t|'MkWitKind AbstractDynamicEntityFamily|])

getGraph ::
       forall a. Eq a
    => (a -> FiniteSet a)
    -> a
    -> FiniteSet a
getGraph getIm a = getGraph1 getIm mempty $ opoint a

getGraph1 ::
       forall a. Eq a
    => (a -> FiniteSet a)
    -> FiniteSet a
    -> FiniteSet a
    -> FiniteSet a
getGraph1 getIm olddoneset currentset =
    if onull currentset
        then olddoneset
        else let
                 newdoneset = union olddoneset currentset
                 newsets = fmap getIm currentset
                 newset = mconcat $ toList newsets
                 diffset = difference newset newdoneset
                 in getGraph1 getIm newdoneset diffset

getImmediateAbstractDynamicSubtypes ::
       [QSubtypeConversionEntry]
    -> AbstractDynamicEntityFamily DynamicEntity
    -> FiniteSet (AbstractDynamicEntityFamily DynamicEntity)
getImmediateAbstractDynamicSubtypes entries famb = let
    matchSCE :: QSubtypeConversionEntry -> Maybe (AbstractDynamicEntityFamily DynamicEntity)
    matchSCE (MkSubtypeConversionEntry _ gta gtb conv) = do
        sfamb@MkAbstractDynamicEntityFamily {} <- getGroundFamily abstractDynamicStorableFamilyWitness gtb
        altIf $ famb == sfamb
        sfama@MkAbstractDynamicEntityFamily {} <- getGroundFamily abstractDynamicStorableFamilyWitness gta
        (Refl, HRefl) <- matchIdentitySubtypeConversion conv
        return sfama
    in setFromList $ mapMaybe matchSCE entries

getImmediateConcreteDynamicSubtypes ::
       [QSubtypeConversionEntry] -> AbstractDynamicEntityFamily DynamicEntity -> [ConcreteDynamicType]
getImmediateConcreteDynamicSubtypes entries famb = let
    matchSCE :: QSubtypeConversionEntry -> Maybe ConcreteDynamicType
    matchSCE (MkSubtypeConversionEntry _ gta gtb conv) = do
        sfamb@MkAbstractDynamicEntityFamily {} <- getGroundFamily abstractDynamicStorableFamilyWitness gtb
        altIf $ famb == sfamb
        MkConcreteDynamicEntityFamily _ cdt <- getGroundFamily concreteDynamicStorableFamilyWitness gta
        (Refl, HRefl) <- matchIdentitySubtypeConversion conv
        return cdt
    in mapMaybe matchSCE entries

abstractDynamicStorableGroundType :: FullName -> TypeIDType tid -> QGroundType '[] DynamicEntity
abstractDynamicStorableGroundType name tid = let
    fam :: AbstractDynamicEntityFamily DynamicEntity
    fam = MkAbstractDynamicEntityFamily tid
    getTypeSet :: Interpreter DynamicTypeSet
    getTypeSet = do
        entries <- getSubtypeConversions
        return $ let
            abss = getGraph (getImmediateAbstractDynamicSubtypes entries) fam
            in setFromList $ mconcat $ fmap (getImmediateConcreteDynamicSubtypes entries) $ toList abss
    props = singleGroundProperty storabilityProperty $ dynamicEntityStorability $ fmap Just getTypeSet
    in (singleGroundType' (MkFamilialType abstractDynamicStorableFamilyWitness fam) props $ exprShowPrec name)
           { qgtGreatestDynamicSupertype =
                 simpleMPolyGreatestDynamicSupertype dynamicEntityStorableGroundType $ do
                     dts <- getTypeSet
                     return $ functionToShim "dynamic-check" $ \de@(MkDynamicEntity dt _) -> ifpure (member dt dts) de
           }

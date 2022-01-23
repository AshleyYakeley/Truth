module Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
    ( makeDynamicEntityTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

intepretSyntaxDynamicEntityConstructor :: SyntaxDynamicEntityConstructor -> Interpreter PinaforeTypeSystem [DynamicType]
intepretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ pure $ mkDynamicType a
intepretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor name) = do
    MkBoundType t <- lookupBoundType name
    case matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t of
        Just (MkADynamicEntityFamily _ dt) -> return $ toList dt
        Nothing -> throw $ InterpretTypeNotDynamicEntityError $ exprShow name

makeDynamicEntityTypeBox ::
       Name -> Markdown -> NonEmpty SyntaxDynamicEntityConstructor -> PinaforeInterpreter PinaforeTypeBox
makeDynamicEntityTypeBox name doc stcons =
    return $ let
        mktype :: DynamicEntityType -> PinaforeBoundType
        mktype t = MkBoundType $ aDynamicEntityGroundType name t
        in mkTypeFixBox name doc mktype $ do
               dt <- for stcons intepretSyntaxDynamicEntityConstructor
               let
                   dts = setFromList $ mconcat $ toList dt
                   tp = aDynamicEntityGroundType name dts
               return $
                   (,) dts $
                   MkCatEndo $
                   MkWMFunction $
                   withSubtypeConversions $
                   pure $
                   MkSubypeConversionEntry tp $ \t -> do
                       Refl <- testEquality (pgtVarianceType t) NilListType
                       MkADynamicEntityFamily _ dts' <- matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t
                       ifpure (isSubsetOf dts' dts) idSubtypeConversion

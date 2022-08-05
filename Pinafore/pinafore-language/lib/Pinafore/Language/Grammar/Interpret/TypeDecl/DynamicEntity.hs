module Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
    ( makeDynamicEntityTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

interpretSyntaxDynamicEntityConstructor ::
       SyntaxDynamicEntityConstructor -> Interpreter PinaforeTypeSystem DynamicEntityType
interpretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ opoint $ mkDynamicType a
interpretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor name) = do
    MkBoundType t <- lookupBoundType name
    case matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t of
        Just (MkADynamicEntityFamily _ det) -> return det
        Nothing -> throw $ InterpretTypeNotDynamicEntityError $ exprShow name

makeDynamicEntityTypeBox ::
       Name -> Markdown -> NonEmpty SyntaxDynamicEntityConstructor -> PinaforeInterpreter (PinaforeFixBox () ())
makeDynamicEntityTypeBox name doc stcons =
    return $ let
        register :: DynamicEntityType -> PinaforeScopeInterpreter ()
        register det = do
            let tp = aDynamicEntityGroundType name det
            registerType name doc tp
        construct :: () -> PinaforeScopeInterpreter (DynamicEntityType, ())
        construct _ = do
            dts <- lift $ for stcons interpretSyntaxDynamicEntityConstructor
            let
                det = mconcat $ toList dts
                tp = aDynamicEntityGroundType name det
            registerSubtypeConversion $
                MkSubtypeConversionEntry tp $ \t -> do
                    Refl <- testEquality (pgtVarianceType t) NilListType
                    MkADynamicEntityFamily _ det' <- matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t
                    ifpure (isSubsetOf det' det) idSubtypeConversion
            return (det, ())
        in mkFixBox register construct

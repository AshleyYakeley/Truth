module Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
    ( makeDynamicEntityTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

interpretSyntaxDynamicEntityConstructor :: SyntaxDynamicEntityConstructor -> QInterpreter DynamicEntityType
interpretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ opoint $ mkDynamicType a
interpretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor name) = do
    MkSomeGroundType t <- lookupBoundType name
    case matchFamilyType aDynamicEntityFamilyWitness $ pgtFamilyType t of
        Just (MkADynamicEntityFamily _ det) -> return det
        Nothing -> throwWithName $ \ntt -> InterpretTypeNotDynamicEntityError $ ntt $ exprShow name

makeDynamicEntityTypeBox :: Name -> Markdown -> NonEmpty SyntaxDynamicEntityConstructor -> QInterpreter (QFixBox () ())
makeDynamicEntityTypeBox name doc stcons =
    return $ let
        register :: DynamicEntityType -> QScopeInterpreter ()
        register det = do
            let tp = aDynamicEntityGroundType name det
            registerType (UnqualifiedFullNameRef name) doc tp
        construct :: () -> QScopeInterpreter (DynamicEntityType, ())
        construct _ = do
            dts <- lift $ for stcons interpretSyntaxDynamicEntityConstructor
            let det = mconcat $ toList dts
            return (det, ())
        in mkFixBox register construct

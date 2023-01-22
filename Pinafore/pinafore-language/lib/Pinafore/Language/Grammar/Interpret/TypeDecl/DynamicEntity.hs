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
interpretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor _ name) = do
    MkSomeGroundType t <- lookupBoundType name
    case matchFamilyType aDynamicStorableFamilyWitness $ qgtFamilyType t of
        Just (MkADynamicStorableFamily _ det) -> return det
        Nothing -> throwWithName $ \ntt -> InterpretTypeNotDynamicEntityError $ ntt $ exprShow name

makeDynamicEntityTypeBox ::
       FullName -> RawMarkdown -> NonEmpty SyntaxDynamicEntityConstructor -> QInterpreter (QFixBox () ())
makeDynamicEntityTypeBox name doc stcons =
    return $ let
        register :: DynamicEntityType -> QScopeInterpreter ()
        register det = do
            let tp = aDynamicStorableGroundType name det
            registerType name doc tp
        construct :: () -> QScopeInterpreter (DynamicEntityType, ())
        construct _ = do
            dts <- lift $ for stcons interpretSyntaxDynamicEntityConstructor
            let det = mconcat $ toList dts
            return (det, ())
        in mkFixBox register construct

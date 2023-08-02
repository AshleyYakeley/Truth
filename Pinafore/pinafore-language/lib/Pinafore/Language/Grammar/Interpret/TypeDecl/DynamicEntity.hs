module Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
    ( makeDynamicEntityTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

interpretSyntaxDynamicEntityConstructor :: SyntaxDynamicEntityConstructor -> QInterpreter DynamicEntityType
interpretSyntaxDynamicEntityConstructor (AnchorSyntaxDynamicEntityConstructor a) = return $ opoint $ mkDynamicType a
interpretSyntaxDynamicEntityConstructor (NameSyntaxDynamicEntityConstructor _ name) = do
    MkSomeGroundType t <- lookupBoundType name
    case getGroundFamily aDynamicStorableFamilyWitness t of
        Just (MkADynamicEntityFamily _ det) -> return det
        Nothing -> throw $ InterpretTypeNotDynamicEntityError $ exprShow name

makeDynamicEntityTypeBox ::
       FullName -> RawMarkdown -> NonEmpty SyntaxDynamicEntityConstructor -> QInterpreter (QFixBox () ())
makeDynamicEntityTypeBox name doc stcons =
    return $ let
        register :: DynamicEntityType -> QScopeBuilder ()
        register det = do
            let tp = aDynamicStorableGroundType name det
            registerType name doc tp
        construct :: () -> QScopeBuilder (DynamicEntityType, ())
        construct _ = do
            dts <- builderLift $ for stcons interpretSyntaxDynamicEntityConstructor
            let det = mconcat $ toList dts
            return (det, ())
        in mkFixBox register construct

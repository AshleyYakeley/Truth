module Pinafore.Language.Grammar.Interpret.TypeDecl.DynamicEntity
    ( makeAbstractDynamicEntityTypeBox
    , makeConcreteDynamicEntityTypeBox
    ) where

import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.Grammar.SyntaxDoc
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

makeAbstractDynamicEntityTypeBox :: FullName -> RawMarkdown -> QInterpreter (QFixBox () ())
makeAbstractDynamicEntityTypeBox name md =
    withNewTypeID $ \tidsym -> let
        register :: () -> QScopeBuilder ()
        register _ = do
            let
                doc = MkDefDoc (typeDocItem name True []) md
                t = abstractDynamicStorableGroundType name tidsym mempty
            registerType name doc t
            registerSubtypeConversion $
                MkSubtypeConversionEntry Verify t dynamicEntityStorableGroundType identitySubtypeConversion
        in return $ mkRegisterFixBox register

makeConcreteDynamicEntityTypeBox :: FullName -> RawMarkdown -> Anchor -> QInterpreter (QFixBox () ())
makeConcreteDynamicEntityTypeBox name md anchor = let
    register :: () -> QScopeBuilder ()
    register _ = do
        let
            doc = MkDefDoc (typeDocItem name True []) md
            t = concreteDynamicStorableGroundType name (mkConcreteDynamicType anchor)
        registerType name doc t
        registerSubtypeConversion $
            MkSubtypeConversionEntry Verify t dynamicEntityStorableGroundType identitySubtypeConversion
    in return $ mkRegisterFixBox register

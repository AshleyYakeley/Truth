module Pinafore.Language.Interpret.TypeDecl.DynamicEntity
    ( makeAbstractDynamicEntityTypeBox
    , makeConcreteDynamicEntityTypeBox
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

dynamicTypeName :: Maybe NamedText
dynamicTypeName = Just $ precNamedText maxBound $ qgtShowType dynamicEntityStorableGroundType

makeAbstractDynamicEntityTypeBox :: FullName -> RawMarkdown -> QInterpreter (QFixBox () ())
makeAbstractDynamicEntityTypeBox name md =
    withNewTypeID $ \tidsym -> let
        register :: () -> QScopeBuilder ()
        register _ = do
            let
                doc = MkDefDoc (typeDocItem name True [] dynamicTypeName) md
                t = abstractDynamicStorableGroundType name tidsym
            registerGroundType name doc t
            registerSubtypeConversion $
                MkSubtypeConversionEntry Verify t dynamicEntityStorableGroundType identitySubtypeConversion
        in return $ mkRegisterFixBox register

makeConcreteDynamicEntityTypeBox :: FullName -> RawMarkdown -> Anchor -> QInterpreter (QFixBox () ())
makeConcreteDynamicEntityTypeBox name md anchor = let
    register :: () -> QScopeBuilder ()
    register _ = do
        let
            doc = MkDefDoc (typeDocItem name True [] dynamicTypeName) md
            t = concreteDynamicStorableGroundType name (mkConcreteDynamicType anchor)
        registerGroundType name doc t
        registerSubtypeConversion $
            MkSubtypeConversionEntry Verify t dynamicEntityStorableGroundType identitySubtypeConversion
    in return $ mkRegisterFixBox register

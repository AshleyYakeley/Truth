module Pinafore.Language.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

makeOpenEntityTypeBox :: FullName -> RawMarkdown -> QInterpreter (QFixBox () ())
makeOpenEntityTypeBox name md =
    withNewTypeID $ \tidsym -> let
        register :: () -> QScopeBuilder ()
        register _ = do
            let
                doc = MkDefDoc (typeDocItem name True []) md
                t = openStorableGroundType $ MkOpenEntityType name tidsym
            registerGroundType name doc t
            registerSubtypeConversion $ MkSubtypeConversionEntry Verify t entityGroundType coerceSubtypeConversion
        in return $ mkRegisterFixBox register

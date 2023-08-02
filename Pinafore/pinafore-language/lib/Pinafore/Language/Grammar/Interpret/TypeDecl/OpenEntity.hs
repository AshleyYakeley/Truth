module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

makeOpenEntityTypeBox :: FullName -> RawMarkdown -> QInterpreter (QFixBox () ())
makeOpenEntityTypeBox name doc =
    withNewTypeID $ \tidsym -> let
        register :: () -> QScopeBuilder ()
        register _ = do
            let t = openStorableGroundType $ MkOpenEntityType name tidsym
            registerType name doc t
            registerSubtypeConversion $ MkSubtypeConversionEntry Verify t entityGroundType coerceSubtypeConversion
        in return $ mkRegisterFixBox register

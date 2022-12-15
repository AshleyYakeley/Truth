module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

makeOpenEntityTypeBox :: FullName -> RawMarkdown -> QInterpreter (QFixBox () ())
makeOpenEntityTypeBox name doc =
    withNewTypeID $ \tidsym -> let
        register :: () -> QScopeInterpreter ()
        register _ = do
            let t = openEntityGroundType $ MkOpenEntityType name tidsym
            registerType name doc t
            registerSubtypeConversion $ MkSubtypeConversionEntry Verify t entityGroundType coerceSubtypeConversion
        in return $ mkRegisterFixBox register

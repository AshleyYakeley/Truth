module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.DefDoc
import Pinafore.Language.Grammar.SyntaxDoc
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

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

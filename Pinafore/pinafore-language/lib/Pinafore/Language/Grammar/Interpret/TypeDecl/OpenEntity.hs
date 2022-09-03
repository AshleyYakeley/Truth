module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

makeOpenEntityTypeBox :: Name -> Markdown -> PinaforeInterpreter (PinaforeFixBox () ())
makeOpenEntityTypeBox name doc =
    withNewTypeID $ \tidsym -> let
        register :: () -> PinaforeScopeInterpreter ()
        register _ = do
            let t = openEntityGroundType $ MkOpenEntityType name tidsym
            registerType name doc t
            registerSubtypeConversion $ MkSubtypeConversionEntry Verify t entityGroundType CoerceSubtypeConversion
        in return $ mkRegisterFixBox register

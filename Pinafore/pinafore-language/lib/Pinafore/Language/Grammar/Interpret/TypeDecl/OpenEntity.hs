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
        register _ = registerType name doc $ openEntityGroundType $ MkOpenEntityType name tidsym
        construct :: () -> PinaforeScopeInterpreter ((), ())
        construct () = return ((), ())
        in return $ mkFixBox register construct

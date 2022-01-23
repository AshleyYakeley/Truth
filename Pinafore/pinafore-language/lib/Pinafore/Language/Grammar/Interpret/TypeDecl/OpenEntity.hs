module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

makeOpenEntityTypeBox :: Name -> Markdown -> PinaforeInterpreter PinaforeTypeBox
makeOpenEntityTypeBox name doc =
    newTypeID $ \tidsym -> let
        mktype _ = MkBoundType $ openEntityGroundType $ MkOpenEntityType name tidsym
        in mkTypeFixBox name doc mktype $ return ((), mempty)

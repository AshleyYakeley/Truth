module Pinafore.Language.Grammar.Interpret.TypeDecl.OpenEntity
    ( makeOpenEntityTypeBox
    ) where

import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

makeOpenEntityType :: Name -> TypeID -> AnyW OpenEntityType
makeOpenEntityType n tid = valueToWitness tid $ \tidsym -> MkAnyW $ MkOpenEntityType n tidsym

makeOpenEntityTypeBox :: Name -> Markdown -> PinaforeInterpreter PinaforeTypeBox
makeOpenEntityTypeBox name doc = do
    tid <- newTypeID
    let
        mktype _ =
            case makeOpenEntityType name tid of
                MkAnyW t -> MkBoundType $ openEntityGroundType t
    return $ mkTypeFixBox name doc mktype $ return ((), id)

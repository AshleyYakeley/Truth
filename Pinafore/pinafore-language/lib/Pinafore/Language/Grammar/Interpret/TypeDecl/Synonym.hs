module Pinafore.Language.Grammar.Interpret.TypeDecl.Synonym
    ( makeSynonymTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

makeSynonymTypeBox ::
       FullName -> RawMarkdown -> Bool -> [SyntaxTypeParameter] -> SyntaxType -> QInterpreter (QFixBox () ())
makeSynonymTypeBox _name _doc _storable _params _bodytype = throw $ KnownIssueError 47 "type synonyms NYI"

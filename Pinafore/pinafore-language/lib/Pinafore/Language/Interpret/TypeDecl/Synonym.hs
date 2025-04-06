module Pinafore.Language.Interpret.TypeDecl.Synonym
    ( makeSynonymTypeBox
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter

makeSynonymTypeBox ::
    FullName ->
    RawMarkdown ->
    Bool ->
    [SyntaxTypeParameter] ->
    SyntaxType ->
    QInterpreter (QFixBox () ())
makeSynonymTypeBox _name _md _storable _params _sparent = throw $ InternalError (Just 47) "Type synonyms NYI"

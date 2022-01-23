module Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox where

import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type PinaforeTypeBox = TypeFixBox PinaforeTypeSystem (CatEndo WMFunction PinaforeInterpreter)

data Constructor w t =
    forall a. MkConstructor Name
                            (ListType w a)
                            (HList a -> t)
                            (t -> Maybe (HList a))

extendConstructor :: Constructor w t -> Constructor w (Either a t)
extendConstructor (MkConstructor n lt at tma) = MkConstructor n lt (Right . at) (\t -> eitherRight t >>= tma)

constructorFreeVariables :: Constructor PinaforeNonpolarType t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ lt _ _) = mconcat $ listTypeToList nonpolarTypeFreeVariables lt

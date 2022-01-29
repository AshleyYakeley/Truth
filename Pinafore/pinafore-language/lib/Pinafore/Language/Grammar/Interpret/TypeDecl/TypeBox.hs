module Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox where

import Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type PinaforeFixBox = TypeFixBox PinaforeTypeSystem

type PinaforeTypeBox = PinaforeFixBox () (CatEndo WMFunction PinaforeInterpreter)

type Constructor :: (Type -> Type) -> Type -> Type
data Constructor w t =
    forall a. MkConstructor Name
                            (w a)
                            (Codec t a)

extendConstructor :: Constructor w t -> Constructor w (Either a t)
extendConstructor (MkConstructor n lt codec) = MkConstructor n lt $ extendRightCodec codec

constructorFreeVariables :: Constructor (HListWit PinaforeNonpolarType) t -> [AnyW SymbolType]
constructorFreeVariables (MkConstructor _ (MkHListWit lt) _) = mconcat $ listTypeToList nonpolarTypeFreeVariables lt

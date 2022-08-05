module Pinafore.Language.Grammar.Interpret.TypeDecl.Constructor where

import Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
import Pinafore.Language.Type
import Shapes

type Constructor :: Type -> (Type -> Type) -> Type -> Type
data Constructor n w t =
    forall a. MkConstructor n
                            (w a)
                            (Codec t a)

extendConstructor :: Constructor n w t -> Constructor n w (Either a t)
extendConstructor (MkConstructor n lt codec) = MkConstructor n lt $ extendRightCodec codec

constructorFreeVariables :: Constructor n (ListProductType PinaforeNonpolarType) t -> [Some SymbolType]
constructorFreeVariables (MkConstructor _ (MkListProductType lt) _) =
    mconcat $ listTypeToList nonpolarTypeFreeVariables lt

module Language.Expression.UVar
    ( UVar
    , unsafeUVarIsomorphism
    , renameUVar
    , varRenamerTGenerateSymbol
    , varRenamerTGenerateSuggestedSymbol
    ) where

import GHC.Exts (Any)
import Language.Expression.Renamer
import Shapes
import Shapes.Unsafe (unsafeIsomorphism)

newtype UVar (name :: Symbol) =
    MkUVar GHC.Exts.Any

unsafeUVarIsomorphism :: Category cat => Isomorphism cat a (UVar name)
unsafeUVarIsomorphism =
    case MkUVar -- hack for unused name warning
          of
        _ -> unsafeIsomorphism

renameUVar ::
       forall m cat name1 r. (Monad m, Category cat)
    => (String -> m String)
    -> SymbolType name1
    -> (forall (name2 :: Symbol). SymbolType name2 -> Isomorphism cat (UVar name1) (UVar name2) -> m r)
    -> m r
renameUVar sf namewit1 cont = do
    newname <- sf $ witnessToValue namewit1
    valueToWitness newname $ \namewit2 -> cont namewit2 unsafeIsomorphism

varRenamerTGenerateSymbol ::
       Monad m => (forall (name :: Symbol). SymbolType name -> VarRenamerT ts m a) -> VarRenamerT ts m a
varRenamerTGenerateSymbol cont = do
    s <- varRenamerTGenerate
    valueToWitness s cont

varRenamerTGenerateSuggestedSymbol ::
       Monad m => String -> (forall (name :: Symbol). SymbolType name -> VarRenamerT ts m a) -> VarRenamerT ts m a
varRenamerTGenerateSuggestedSymbol name cont = do
    name' <- varRenamerTGenerateSuggested name
    valueToWitness name' cont

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
import Unsafe.Coerce

newtype UVar (name :: Symbol) =
    MkUVar GHC.Exts.Any

unsafeRefl :: forall a b. a :~: b
unsafeRefl = unsafeCoerce Refl

unsafeCat ::
       forall cat a b. Category cat
    => cat a b
unsafeCat =
    case unsafeRefl @a @b of
        Refl -> id

unsafeToUVar :: Category cat => cat a (UVar name)
unsafeToUVar =
    case MkUVar -- hack for unused name warning
          of
        _ -> unsafeCat

unsafeFromUVar :: Category cat => cat (UVar name) a
unsafeFromUVar = unsafeCat

unsafeUVarIsomorphism :: Category cat => Isomorphism cat a (UVar name)
unsafeUVarIsomorphism = MkIsomorphism unsafeToUVar unsafeFromUVar

renameUVar ::
       forall m cat name1 r. (Monad m, Category cat)
    => (String -> m String)
    -> SymbolType name1
    -> (forall (name2 :: Symbol). SymbolType name2 -> Isomorphism cat (UVar name1) (UVar name2) -> m r)
    -> m r
renameUVar sf namewit1 cont = do
    newname <- sf $ witnessToValue namewit1
    valueToWitness newname $ \namewit2 -> cont namewit2 (MkIsomorphism unsafeCat unsafeCat)

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

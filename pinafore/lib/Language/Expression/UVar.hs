module Language.Expression.UVar
    ( UVar
    , unsafeUVarBijection
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

unsafeRenameAnybox :: UVar name1 -> UVar name2
unsafeRenameAnybox (MkUVar a) = MkUVar a

unsafeToUVar :: a -> UVar name
unsafeToUVar a = MkUVar $ unsafeCoerce a

unsafeFromUVar :: UVar name -> a
unsafeFromUVar (MkUVar a) = unsafeCoerce a

unsafeUVarBijection :: Bijection a (UVar name)
unsafeUVarBijection = MkBijection unsafeToUVar unsafeFromUVar

renameUVar ::
       Monad m
    => (String -> m String)
    -> SymbolType name1
    -> (forall (name2 :: Symbol). SymbolType name2 -> Bijection (UVar name1) (UVar name2) -> m r)
    -> m r
renameUVar sf namewit1 cont = do
    newname <- sf $ witnessToValue namewit1
    valueToWitness newname $ \namewit2 -> cont namewit2 (MkBijection unsafeRenameAnybox unsafeRenameAnybox)

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

module Language.Expression.UVar
    ( UVar
    , unsafeUVarBijection
    , renameUVar
    , varRenamerGenerateSymbol
    , varRenamerGenerateSuggestedSymbol
    ) where

import GHC.Exts (Any)
import GHC.TypeLits
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
    -> SymbolWitness name1
    -> (forall (name2 :: Symbol). SymbolWitness name2 -> Bijection (UVar name1) (UVar name2) -> m r)
    -> m r
renameUVar sf namewit1 cont = do
    newname <- sf $ fromSymbolWitness namewit1
    toSymbolWitness newname $ \namewit2 -> cont namewit2 (MkBijection unsafeRenameAnybox unsafeRenameAnybox)

varRenamerGenerateSymbol ::
       Monad m => (forall (name :: Symbol). SymbolWitness name -> VarRenamer ts m a) -> VarRenamer ts m a
varRenamerGenerateSymbol cont = do
    s <- varRenamerGenerate
    toSymbolWitness s cont

varRenamerGenerateSuggestedSymbol ::
       Monad m => String -> (forall (name :: Symbol). SymbolWitness name -> VarRenamer ts m a) -> VarRenamer ts m a
varRenamerGenerateSuggestedSymbol name cont = do
    name' <- varRenamerGenerateSuggested name
    toSymbolWitness name' cont

module Language.Expression.UVar
    ( UVar
    , unsafeUVarIsomorphism
    , renameUVar
    , varRenamerTGenerateSymbol
    , varRenamerTGenerateSuggestedSymbol
    ) where

import Data.Shim.JoinMeet
import GHC.Exts (Any)
import Language.Expression.Renamer
import Shapes
import Unsafe.Coerce

newtype UVar (name :: Symbol) =
    MkUVar GHC.Exts.Any

unsafeAnyEq :: forall a. GHC.Exts.Any :~: a
unsafeAnyEq = unsafeCoerce Refl

unsafeCat1 ::
       forall cat a. Category cat
    => cat GHC.Exts.Any a
unsafeCat1 =
    case unsafeAnyEq @a of
        Refl -> id

unsafeCat2 ::
       forall cat a. Category cat
    => cat a GHC.Exts.Any
unsafeCat2 =
    case unsafeAnyEq @a of
        Refl -> id

unsafeToUVar :: (Category cat, EnhancedFunction cat) => cat a (UVar name)
unsafeToUVar =
    case MkUVar -- hack for unused name warning
          of
        _ -> coerceEnhanced . unsafeCat2

unsafeFromUVar :: (Category cat, EnhancedFunction cat) => cat (UVar name) a
unsafeFromUVar = unsafeCat1 . coerceEnhanced

unsafeUVarIsomorphism :: (Category cat, EnhancedFunction cat) => Isomorphism cat a (UVar name)
unsafeUVarIsomorphism = MkIsomorphism unsafeToUVar unsafeFromUVar

renameUVar ::
       forall m cat name1 r. (Monad m, EnhancedFunction cat)
    => (String -> m String)
    -> SymbolType name1
    -> (forall (name2 :: Symbol). SymbolType name2 -> Isomorphism cat (UVar name1) (UVar name2) -> m r)
    -> m r
renameUVar sf namewit1 cont = do
    newname <- sf $ witnessToValue namewit1
    valueToWitness newname $ \namewit2 -> cont namewit2 (MkIsomorphism coerceEnhanced coerceEnhanced)

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

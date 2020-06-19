module Language.Expression.TypeVariable
    ( UVar
    , unsafeUVarIsomorphism
    , varRenamerTGenerateSymbol
    , varRenamerTGenerateSuggestedSymbol
    , varNamespaceTRenameUVar
    , varNamespaceTAddUVars
    , varNamespaceTLocalUVar
    ) where

import Data.Shim
import qualified GHC.Exts (Any)
import Language.Expression.Common
import Shapes
import Shapes.Unsafe (unsafeIsomorphism)

newtype UVar (name :: Symbol) =
    MkUVar GHC.Exts.Any

unsafeUVarIsomorphism ::
       forall (cat :: ShimKind Type) (name :: Symbol) (a :: Type). Category cat
    => Isomorphism cat a (UVar name)
unsafeUVarIsomorphism =
    case MkUVar -- hack for unused name warning
          of
        _ -> unsafeIsomorphism

renameUVar ::
       forall m (cat :: ShimKind Type) name1 r. (Monad m, Category cat)
    => String
    -> SymbolType name1
    -> (forall (newname :: Symbol). SymbolType newname -> Isomorphism cat (UVar name1) (UVar newname) -> m r)
    -> m r
renameUVar newname _ call = valueToWitness newname $ \namewit2 -> call namewit2 unsafeUVarIsomorphism

renameUVars ::
       forall m (cat :: ShimKind Type) (names :: [Symbol]) r. (Monad m, Category cat)
    => String
    -> ListType SymbolType names
    -> (forall (newname :: Symbol).
                SymbolType newname -> ListType (Compose (Isomorphism cat (UVar newname)) UVar) names -> m r)
    -> m r
renameUVars newname names call =
    valueToWitness newname $ \namewit2 -> call namewit2 $ mapListType (\_ -> Compose unsafeUVarIsomorphism) names

varRenamerTGenerateSymbol ::
       Monad m => (forall (name :: Symbol). SymbolType name -> VarRenamerT ts m a) -> VarRenamerT ts m a
varRenamerTGenerateSymbol cont = do
    s <- varRenamerTGenerate []
    valueToWitness s cont

varRenamerTGenerateSuggestedSymbol ::
       Monad m => String -> (forall (name :: Symbol). SymbolType name -> VarRenamerT ts m a) -> VarRenamerT ts m a
varRenamerTGenerateSuggestedSymbol name cont = do
    name' <- varRenamerTGenerate [name]
    valueToWitness name' cont

varNamespaceTRenameUVar ::
       forall ts m (cat :: ShimKind Type) name1 r. (Monad m, Category cat)
    => SymbolType name1
    -> (forall (newname :: Symbol).
                SymbolType newname -> Isomorphism cat (UVar name1) (UVar newname) -> VarNamespaceT ts (VarRenamerT ts m) r)
    -> VarNamespaceT ts (VarRenamerT ts m) r
varNamespaceTRenameUVar namewit1 call = do
    newname <- varNamespaceTRename $ witnessToValue namewit1
    renameUVar newname namewit1 call

varNamespaceTAddUVars ::
       forall ts m (cat :: ShimKind Type) (names :: [Symbol]) r. (Monad m, Category cat)
    => ListType SymbolType names
    -> (forall (newname :: Symbol).
                SymbolType newname -> ListType (Compose (Isomorphism cat (UVar newname)) UVar) names -> VarNamespaceT ts (VarRenamerT ts m) r)
    -> VarNamespaceT ts (VarRenamerT ts m) r
varNamespaceTAddUVars names call = do
    newname <- varNamespaceTAddNames $ listTypeToList witnessToValue names
    renameUVars newname names call

varNamespaceTLocalUVar ::
       forall ts m cat name1 r. (Monad m, Category cat)
    => SymbolType name1
    -> (forall (newname :: Symbol).
                SymbolType newname -> Isomorphism cat (UVar name1) (UVar newname) -> VarNamespaceT ts (VarRenamerT ts m) r)
    -> VarNamespaceT ts (VarRenamerT ts m) r
varNamespaceTLocalUVar namewit1 call =
    varNamespaceTLocal (witnessToValue namewit1) $ \newname -> renameUVar newname namewit1 call

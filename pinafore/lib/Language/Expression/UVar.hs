module Language.Expression.UVar
    ( UVar
    , unsafeUVarBijection
    , renameUVar
    ) where

import GHC.Exts (Any)
import GHC.TypeLits
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
       (String -> String)
    -> SymbolWitness name1
    -> (forall (name2 :: Symbol). SymbolWitness name2 -> Bijection (UVar name1) (UVar name2) -> r)
    -> r
renameUVar sf namewit1 cont =
    toSymbolWitness (sf $ fromSymbolWitness namewit1) $ \namewit2 ->
        cont namewit2 (MkBijection unsafeRenameAnybox unsafeRenameAnybox)

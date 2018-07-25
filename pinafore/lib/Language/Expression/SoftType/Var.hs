module Language.Expression.SoftType.Var where

import GHC.Exts (Any)
import GHC.TypeLits
import Shapes
import Unsafe.Coerce

newtype SVar (name :: Symbol) =
    MkSVar GHC.Exts.Any

unsafeRenameAnybox :: SVar name1 -> SVar name2
unsafeRenameAnybox (MkSVar a) = MkSVar a

unsafeToSVar :: a -> SVar name
unsafeToSVar a = MkSVar $ unsafeCoerce a

unsafeFromSVar :: SVar name -> a
unsafeFromSVar (MkSVar a) = unsafeCoerce a

renameSVar ::
       (String -> String)
    -> SymbolWitness name1
    -> (forall (name2 :: Symbol). SymbolWitness name2 -> Bijection (SVar name1) (SVar name2) -> r)
    -> r
renameSVar sf namewit1 cont =
    toSymbolWitness (sf $ fromSymbolWitness namewit1) $ \namewit2 ->
        cont namewit2 (MkBijection unsafeRenameAnybox unsafeRenameAnybox)

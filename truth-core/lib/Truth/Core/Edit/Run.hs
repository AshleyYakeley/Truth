module Truth.Core.Edit.Run where

import Truth.Core.Import

data RunnableT2 f (a :: k) (b :: k) =
    forall t. MonadTransUntrans t =>
                  MkRunnableT2 (Untrans t)
                               (f t a b)

class RunnableCategory (f :: ((Type -> Type) -> (Type -> Type)) -> k -> k -> Type) where
    ucId :: forall a. f IdentityT a a
    ucCompose ::
           forall tab tbc a b c. (MonadTransUntrans tab, MonadTransUntrans tbc)
        => f tbc b c
        -> f tab a b
        -> f (ComposeT tbc tab) a c

instance RunnableCategory f => Category (RunnableT2 f) where
    id = MkRunnableT2 identityUntrans ucId
    (MkRunnableT2 unliftBC fBC) . (MkRunnableT2 unliftAB fAB) =
        MkRunnableT2 (composeUntrans unliftBC unliftAB) (ucCompose fBC fAB)

type TransLift t1 t2 = forall m. Monad m => MFunction (t1 m) (t2 m)

joinUnlifts ::
       (forall t1 t2.
            (MonadTransUntrans t1, MonadTransUntrans t2) => f1 t1 a1 b1 -> f2 t2 a2 b2 -> f3 (ComposeT t1 t2) a3 b3)
    -> RunnableT2 f1 a1 b1
    -> RunnableT2 f2 a2 b2
    -> RunnableT2 f3 a3 b3
joinUnlifts call (MkRunnableT2 unlift1 open1) (MkRunnableT2 unlift2 open2) =
    MkRunnableT2 (composeUntrans unlift1 unlift2) $ call open1 open2

data PairUnlift f1 f2 (t :: (Type -> Type) -> (Type -> Type)) (a :: k) (b :: k) =
    MkPairUnlift (f1 t a b)
                 (f2 t a b)

class Unliftable (f :: ((Type -> Type) -> (Type -> Type)) -> k -> k -> Type) where
    fmapUnliftable :: forall a b t1 t2. TransLift t1 t2 -> f t1 a b -> f t2 a b

joinUnliftables ::
       (Unliftable f1, Unliftable f2)
    => (forall t. MonadTransUntrans t => f1 t a1 b1 -> f2 t a2 b2 -> f3 t a3 b3)
    -> RunnableT2 f1 a1 b1
    -> RunnableT2 f2 a2 b2
    -> RunnableT2 f3 a3 b3
joinUnliftables call =
    joinUnlifts $ \open1 open2 -> call (fmapUnliftable lift1ComposeT open1) (fmapUnliftable lift2ComposeT' open2)

pairUnlift ::
       (Unliftable f1, Unliftable f2) => RunnableT2 f1 a b -> RunnableT2 f2 a b -> RunnableT2 (PairUnlift f1 f2) a b
pairUnlift = joinUnliftables MkPairUnlift

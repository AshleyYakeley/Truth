{-# OPTIONS -fno-warn-redundant-constraints #-}

module Truth.Core.Edit.Unlift where

import Truth.Core.Import

type Unlift (t :: (* -> *) -> * -> *)
     = forall (m :: * -> *) (r :: *). MonadUnliftIO m =>
                                          t m r -> m r

identityUnlift :: Unlift IdentityT
identityUnlift = runIdentityT

composeUnlift :: MonadTransUnlift tb => Unlift ta -> Unlift tb -> Unlift (ComposeT ta tb)
composeUnlift ua ub (MkComposeT tatbma) = ub $ withTransConstraintTM @MonadUnliftIO $ ua tatbma

mvarUnlift :: MVar s -> Unlift (StateT s)
mvarUnlift var (StateT smr) = liftIOWithUnlift $ \unlift -> modifyMVar var $ \olds -> unlift $ fmap swap $ smr olds

data CloseUnlift f (a :: k) (b :: k) =
    forall t. MonadTransUnlift t =>
              MkCloseUnlift (Unlift t)
                            (f t a b)

class UnliftCategory (f :: ((* -> *) -> (* -> *)) -> k -> k -> *) where
    type UnliftCategoryConstraint f (a :: k) :: Constraint
    ucId ::
           forall a. UnliftCategoryConstraint f (a :: k)
        => f IdentityT a a
    ucCompose ::
           forall tab tbc a b c.
           ( UnliftCategoryConstraint f a
           , UnliftCategoryConstraint f b
           , UnliftCategoryConstraint f c
           , MonadTransUnlift tab
           , MonadTransUnlift tbc
           )
        => f tbc b c
        -> f tab a b
        -> f (ComposeT tbc tab) a c

instance UnliftCategory f => ConstrainedCategory (CloseUnlift f) where
    type CategoryConstraint (CloseUnlift f) a = UnliftCategoryConstraint f a
    cid = MkCloseUnlift identityUnlift ucId
    (MkCloseUnlift unliftBC fBC) <.> (MkCloseUnlift unliftAB fAB) =
        MkCloseUnlift (composeUnlift unliftBC unliftAB) (ucCompose fBC fAB)

data PairUnlift f1 f2 (t :: (* -> *) -> (* -> *)) (a :: k) (b :: k) =
    MkPairUnlift (f1 t a b)
                 (f2 t a b)

class Unliftable (f :: ((* -> *) -> (* -> *)) -> k -> k -> *) where
    fmapUnliftable ::
           forall a b t1 t2.
           (forall m x. Monad m =>
                            t1 m x -> t2 m x)
        -> f t1 a b
        -> f t2 a b

joinUnlifts ::
       (Unliftable f1, Unliftable f2)
    => (forall t. MonadTransUnlift t =>
                      Unlift t -> f1 t a1 b1 -> f2 t a2 b2 -> r)
    -> CloseUnlift f1 a1 b1
    -> CloseUnlift f2 a2 b2
    -> r
joinUnlifts call (MkCloseUnlift unlift1 open1) (MkCloseUnlift unlift2 open2) =
    call (composeUnlift unlift1 unlift2) (fmapUnliftable lift1ComposeT open1) (fmapUnliftable lift2ComposeT' open2)

pairUnlift ::
       (Unliftable f1, Unliftable f2) => CloseUnlift f1 a b -> CloseUnlift f2 a b -> CloseUnlift (PairUnlift f1 f2) a b
pairUnlift = joinUnlifts $ \unlift f1' f2' -> MkCloseUnlift unlift $ MkPairUnlift f1' f2'
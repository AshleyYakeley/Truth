module Truth.Core.Edit.Unlift where

import Truth.Core.Import

type Unlift (t :: (* -> *) -> * -> *) = forall (m :: * -> *) (r :: *). t m r -> m r

identityUnlift :: Unlift IdentityT
identityUnlift = runIdentityT

composeUnlift :: Unlift ta -> Unlift tb -> Unlift (ComposeT ta tb)
composeUnlift ua ub (MkComposeT tatbma) = ub $ ua tatbma

data CloseUnlift f edita editb =
    forall t. MonadTransUnlift t =>
              MkCloseUnlift (Unlift t)
                            (f t edita editb)

class UnliftCategory (f :: ((* -> *) -> (* -> *)) -> * -> * -> *) where
    type UnliftCategoryConstraint f (a :: *) :: Constraint
    ucId ::
           forall a. UnliftCategoryConstraint f a
        => f IdentityT a a
    ucCompose ::
           forall tab tbc a b c.
           ( UnliftCategoryConstraint f a
           , UnliftCategoryConstraint f b
           , UnliftCategoryConstraint f c
           , MonadTransConstraint MonadIO tab
           , MonadTransConstraint MonadIO tbc
           )
        => f tbc b c
        -> f tab a b
        -> f (ComposeT tbc tab) a c

instance UnliftCategory f => ConstrainedCategory (CloseUnlift f) where
    type CategoryConstraint (CloseUnlift f) a = UnliftCategoryConstraint f a
    cid = MkCloseUnlift identityUnlift ucId
    (MkCloseUnlift unliftBC fBC) <.> (MkCloseUnlift unliftAB fAB) =
        MkCloseUnlift (composeUnlift unliftBC unliftAB) (ucCompose fBC fAB)

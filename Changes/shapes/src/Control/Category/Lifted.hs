module Control.Category.Lifted where

import Data.KindMorphism
import Shapes.Import

newtype LiftedCategory (cat :: kq -> kq -> Type) (f :: kp -> kq) (a :: kp) (b :: kp) =
    MkLiftedCategory (cat (f a) (f b))

instance forall kp kq (cat :: kq -> kq -> Type) (f :: kp -> kq). Category cat => Category (LiftedCategory cat f) where
    id = MkLiftedCategory id
    MkLiftedCategory p . MkLiftedCategory q = MkLiftedCategory $ p . q

instance forall kp kq (cat :: kq -> kq -> Type) (f :: kp -> kq). ( Representative (KindWitness kq)
         , InKind f
         , InCategory cat
         ) => InCategory (LiftedCategory cat f) where
    cid = functionKindWitness (inKind @_ @f) $ MkLiftedCategory cid
    MkLiftedCategory p <.> MkLiftedCategory q = functionKindWitness (inKind @_ @f) $ MkLiftedCategory $ p <.> q

module Pinafore.Base.Order where

import Shapes

type Order t = t -> t -> Ordering

noOrder :: Order a
noOrder _ _ = EQ

reverseOrder :: forall a. Order a -> Order a
reverseOrder o p q = o q p

joinOrderings :: Ordering -> Ordering -> Ordering
joinOrderings EQ ob = ob
joinOrderings oa _ = oa

joinOrders :: Order a -> Order a -> Order a
joinOrders oa ob p q = joinOrderings (oa p q) (ob p q)

concatOrders :: forall a. [Order a] -> Order a
concatOrders [] = noOrder
concatOrders (o:oo) = joinOrders o $ concatOrders oo

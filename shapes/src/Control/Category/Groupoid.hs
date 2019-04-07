module Control.Category.Groupoid where

import Shapes.Import

class Category cat => Groupoid (cat :: k -> k -> Type) where
    invert :: cat a b -> cat b a

instance Groupoid (:~:) where
    invert Refl = Refl

instance Groupoid (:~~:) where
    invert HRefl = HRefl

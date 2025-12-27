module Data.SpecialCase.SpecialCase where

import Shapes.Import

class HasSpecialCase "general" t => HasSpecialCases (t :: Type) where
    unGeneral :: t -> GeneralCase t

type GeneralCase t = SpecialCase "general" t

mkGeneral :: HasSpecialCases t => GeneralCase t -> t
mkGeneral = mkSpecial @"general"

class HasSpecialCases t => HasSpecialCase (id :: Symbol) (t :: Type) where
    type SpecialCase id t :: Type
    mkSpecial :: SpecialCase id t -> t
    checkSpecial :: t -> Maybe (SpecialCase id t)

pattern General :: forall t. HasSpecialCases t => GeneralCase t -> t
pattern General c <- (unGeneral -> c)
    where
        General c = mkGeneral c

{-# COMPLETE General #-}

pattern Special :: forall id t. HasSpecialCase id t => SpecialCase id t -> t
pattern Special c <- (checkSpecial @id -> Just c)
    where
        Special c = mkSpecial @id c

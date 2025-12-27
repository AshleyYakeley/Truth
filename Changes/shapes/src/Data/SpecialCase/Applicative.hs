module Data.SpecialCase.Applicative
    ( ApplicativeOrConst
    )
where

import Data.SpecialCase.SpecialCase
import Shapes.Import

data ApplicativeOrConst m a
    = ConstApplicativeOrConst a
    | ApplicativeApplicativeOrConst (m a)

instance Applicative m => HasSpecialCases (ApplicativeOrConst m a) where
    unGeneral = \case
        ConstApplicativeOrConst a -> pure a
        ApplicativeApplicativeOrConst ma -> ma

instance Applicative m => HasSpecialCase "general" (ApplicativeOrConst m a) where
    type SpecialCase "general" (ApplicativeOrConst m a) = m a
    mkSpecial = ApplicativeApplicativeOrConst
    checkSpecial = Just . unGeneral

instance Applicative m => HasSpecialCase "const" (ApplicativeOrConst m a) where
    type SpecialCase "const" (ApplicativeOrConst m a) = a
    mkSpecial = ConstApplicativeOrConst
    checkSpecial = \case
        ConstApplicativeOrConst a -> Just a
        _ -> Nothing

instance Applicative m => Functor (ApplicativeOrConst m) where
    fmap ab ca = case ca of
        Special @"const" a -> Special @"const" $ ab a
        General ma -> General $ fmap ab ma

instance Applicative m => Applicative (ApplicativeOrConst m) where
    pure = Special @"const"
    cab <*> ca = case cab of
        Special @"const" ab -> fmap ab ca
        General mab -> General $ mab <*> unGeneral ca

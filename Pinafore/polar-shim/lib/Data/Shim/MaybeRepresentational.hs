module Data.Shim.MaybeRepresentational where

import Shapes

class MaybeRepresentational (f :: kp -> kq) where
    maybeRepresentational :: Maybe (Dict (RepresentationalRole f))

instance MaybeRepresentational Maybe where
    maybeRepresentational = Just Dict

instance MaybeRepresentational [] where
    maybeRepresentational = Just Dict

instance MaybeRepresentational NonEmpty where
    maybeRepresentational = Just Dict

instance MaybeRepresentational ((->) a) where
    maybeRepresentational = Just Dict

instance MaybeRepresentational ((,) a) where
    maybeRepresentational = Just Dict

instance MaybeRepresentational (Either a) where
    maybeRepresentational = Just Dict

instance MaybeRepresentational (->) where
    maybeRepresentational = Just Dict

instance MaybeRepresentational (,) where
    maybeRepresentational = Just Dict

instance MaybeRepresentational Either where
    maybeRepresentational = Just Dict

instance MaybeRepresentational Vector where
    maybeRepresentational = Just Dict

instance MaybeRepresentational Task where
    maybeRepresentational = Just Dict

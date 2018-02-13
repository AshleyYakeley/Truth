module Data.HasNewValue where

import Data.Result
import Shapes.Import

class HasNewValue a where
    newValue :: a

instance HasNewValue () where
    newValue = ()

instance HasNewValue Bool where
    newValue = False

instance HasNewValue Int where
    newValue = 0

instance HasNewValue ByteString where
    newValue = mempty

instance HasNewValue Word8 where
    newValue = 0

instance HasNewValue Char where
    newValue = '\0'

instance HasNewValue [a] where
    newValue = []

instance HasNewValue (Maybe a) where
    newValue = Nothing

instance (HasNewValue a) => HasNewValue (Result err a) where
    newValue = SuccessResult newValue

instance HasNewValue Text where
    newValue = mempty

class HasNewValue1 p where
    newValue1 ::
           forall a r. (HasNewValue a)
        => ((HasNewValue (p a)) =>
                r)
        -> r

instance HasNewValue1 [] where
    newValue1 r = r

instance HasNewValue1 Maybe where
    newValue1 r = r

instance HasNewValue1 (Result err) where
    newValue1 r = r

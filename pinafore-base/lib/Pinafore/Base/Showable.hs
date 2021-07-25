module Pinafore.Base.Showable where

import Data.Time
import Shapes
import Shapes.Numeric

class TextShow t where
    -- | NOT the same as 'toText'.
    textShow :: t -> Text
    default textShow :: Show t => t -> Text
    textShow v = pack $ show v

instance TextShow Text

instance TextShow Bool

instance TextShow Ordering

instance TextShow Integer

instance TextShow Rational

instance TextShow Double

instance TextShow ()

instance TextShow NominalDiffTime

instance TextShow UTCTime

instance TextShow Day

instance TextShow TimeOfDay

instance TextShow LocalTime

instance TextShow a => TextShow (Maybe a) where
    textShow Nothing = "Nothing"
    textShow (Just a) = "Just " <> textShow a

instance TextShow a => TextShow [a] where
    textShow aa = "[" <> intercalate ", " (fmap textShow aa) <> "]"

instance (TextShow a, TextShow b) => TextShow (a, b) where
    textShow (a, b) = "(" <> textShow a <> ", " <> textShow b <> ")"

instance (TextShow a, TextShow b) => TextShow (Either a b) where
    textShow (Left a) = "Left " <> textShow a
    textShow (Right b) = "Right " <> textShow b

newtype Showable =
    MkShowable Text

instance TextShow Showable where
    textShow (MkShowable t) = t

instance Show Showable where
    show v = unpack $ textShow v

textShowable :: TextShow t => t -> Showable
textShowable v = MkShowable $ textShow v

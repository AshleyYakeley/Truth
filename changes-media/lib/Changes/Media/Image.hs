module Changes.Media.Image where

import Changes.Core
import Codec.Picture.Types
import Shapes

type ImageRead :: Type -> Type -> Type
data ImageRead px t where
    SizeImageRead :: ImageRead px (Int, Int)
    WholeImageRead :: ImageRead px (Image px)

instance SubjectReader (ImageRead px) where
    type ReaderSubject (ImageRead px) = Image px
    subjectToRead image SizeImageRead = (imageWidth image, imageHeight image)
    subjectToRead image WholeImageRead = image

instance FullSubjectReader (ImageRead px) where
    readableToSubject rd = rd WholeImageRead

type ImageUpdate :: Type -> Type
data ImageUpdate edit
    = SomeImageUpdate
    | RectImageUpdate Int
                      Int
                      Int
                      Int

type instance UpdateEdit (ImageUpdate edit) = edit

type ConstImageUpdate px = ImageUpdate (ConstEdit (ImageRead px))

instance IsUpdate (ConstImageUpdate px) where
    editUpdate = never

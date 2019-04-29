module Pinafore.Language.Type.Inverted
    ( invertedSubtype
    ) where

import Language.Expression.Dolan
import Language.Expression.Polarity
import Pinafore.Language.Error
import Pinafore.Language.Show
import Pinafore.Language.Type.Subtype
import Pinafore.Language.Type.Type
import Shapes

invertedContext :: SubtypeContext baseedit (PinaforeTypeCheck baseedit) 'Negative 'Positive
invertedContext = MkSubtypeContext {subtypeTypes = subtypeTT, subtypeLift = lift, subtypeInverted = invertedContext}

subtypeSS ::
       PinaforeSingularType baseedit 'Negative p
    -> PinaforeSingularType baseedit 'Positive q
    -> PinaforeTypeCheck baseedit (p -> q)
subtypeSS (VarPinaforeSingularType np) (VarPinaforeSingularType nq)
    | Just Refl <- testEquality np nq = return id
subtypeSS (GroundPinaforeSingularType gp argsp) (GroundPinaforeSingularType gq argsq) =
    subtypeGroundTypes invertedContext gp argsp gq argsq
subtypeSS _ _ = empty

subtypeST ::
       PinaforeSingularType baseedit 'Negative p
    -> PinaforeType baseedit 'Positive q
    -> PinaforeTypeCheck baseedit (p -> q)
subtypeST _ NilPinaforeType = empty
subtypeST sp (ConsPinaforeType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS sp sq) <|> fmap (\conv -> join2 . conv) (subtypeST sp tq)

subtypeTT ::
       PinaforeType baseedit 'Negative p -> PinaforeType baseedit 'Positive q -> PinaforeTypeCheck baseedit (p -> q)
subtypeTT NilPinaforeType _ = empty
subtypeTT (ConsPinaforeType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeST sp tq) <|> fmap (\conv -> conv . meet2) (subtypeTT tp tq)

invertedSubtype ::
       PinaforeType baseedit 'Negative p -> PinaforeType baseedit 'Positive q -> PinaforeTypeCheck baseedit (p -> q)
invertedSubtype tp tq = subtypeTT tp tq <|> throwError (TypeConvertInverseError (exprShow tp) (exprShow tq))

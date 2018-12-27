module Pinafore.Language.Type.Inverted
    ( invertedSubtype
    ) where

import Language.Expression.Dolan
import Pinafore.Language.Type.Subtype
import Pinafore.Language.Type.Type
import Shapes

invertedContext :: SubtypeContext baseedit (PinaforeTypeCheck baseedit) 'NegativePolarity 'PositivePolarity
invertedContext = MkSubtypeContext {subtypeTypes = subtypeTT, subtypeLift = lift, subtypeInverted = invertedContext}

subtypeSS ::
       PinaforeSingularType baseedit 'NegativePolarity p
    -> PinaforeSingularType baseedit 'PositivePolarity q
    -> PinaforeTypeCheck baseedit (p -> q)
subtypeSS (VarPinaforeSingularType np) (VarPinaforeSingularType nq)
    | Just Refl <- testEquality np nq = return id
subtypeSS (GroundPinaforeSingularType gp argsp) (GroundPinaforeSingularType gq argsq) =
    subtypeGroundTypes invertedContext gp argsp gq argsq
subtypeSS _ _ = fail ""

subtypeST ::
       PinaforeSingularType baseedit 'NegativePolarity p
    -> PinaforeType baseedit 'PositivePolarity q
    -> PinaforeTypeCheck baseedit (p -> q)
subtypeST _ NilPinaforeType = fail ""
subtypeST sp (ConsPinaforeType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS sp sq) <|> fmap (\conv -> join2 . conv) (subtypeST sp tq)

subtypeTT ::
       PinaforeType baseedit 'NegativePolarity p
    -> PinaforeType baseedit 'PositivePolarity q
    -> PinaforeTypeCheck baseedit (p -> q)
subtypeTT NilPinaforeType _ = fail ""
subtypeTT (ConsPinaforeType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeST sp tq) <|> fmap (\conv -> conv . meet2) (subtypeTT tp tq)

invertedSubtype ::
       PinaforeType baseedit 'NegativePolarity p
    -> PinaforeType baseedit 'PositivePolarity q
    -> PinaforeTypeCheck baseedit (p -> q)
invertedSubtype tp tq = subtypeTT tp tq <|> fail ("cannot convert " <> show tp <> " to " <> show tq)

module Pinafore.Language.TypeSystem.Inverted
    ( invertedSubtype
    ) where

import Data.Shim
import Pinafore.Language.Error
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.TypeSystem.Subtype
import Pinafore.Language.TypeSystem.Type
import Shapes

invertedContext :: SubtypeContext (PinaforeTypeCheck) 'Negative 'Positive
invertedContext = MkSubtypeContext {subtypeTypes = subtypeTT, subtypeLift = lift, subtypeInverted = invertedContext}

subtypeSS :: PinaforeSingularType 'Negative p -> PinaforeSingularType 'Positive q -> PinaforeTypeCheck (JMShim p q)
subtypeSS (VarPinaforeSingularType np) (VarPinaforeSingularType nq)
    | Just Refl <- testEquality np nq = return id
subtypeSS (GroundPinaforeSingularType gp argsp) (GroundPinaforeSingularType gq argsq) =
    subtypeGroundTypes invertedContext gp argsp gq argsq
subtypeSS _ _ = empty

subtypeST :: PinaforeSingularType 'Negative p -> PinaforeType 'Positive q -> PinaforeTypeCheck (JMShim p q)
subtypeST _ NilPinaforeType = empty
subtypeST sp (ConsPinaforeType sq tq) =
    fmap (\conv -> join1 . conv) (subtypeSS sp sq) <|> fmap (\conv -> join2 . conv) (subtypeST sp tq)

subtypeTT :: PinaforeType 'Negative p -> PinaforeType 'Positive q -> PinaforeTypeCheck (JMShim p q)
subtypeTT NilPinaforeType _ = empty
subtypeTT (ConsPinaforeType sp tp) tq =
    fmap (\conv -> conv . meet1) (subtypeST sp tq) <|> fmap (\conv -> conv . meet2) (subtypeTT tp tq)

invertedSubtype :: PinaforeType 'Negative p -> PinaforeType 'Positive q -> PinaforeTypeCheck (JMShim p q)
invertedSubtype tp tq = subtypeTT tp tq <|> throw (TypeConvertInverseError (exprShow tp) (exprShow tq))
